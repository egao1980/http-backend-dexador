# http-backend-dexador

MIT. Sync [`http-protocol`](https://github.com/egao1980/http-protocol) backend over [dexador](https://github.com/fukamachi/dexador). **OCI: 0.2.0.**

| Concern | Behavior |
|---------|----------|
| `Accept-Encoding` | `default-accept-encoding`. The encoding backends are soft-loaded once per image through http-protocol's cached probe. |
| Response CE | Auto-decode. Dexador already unwraps gzip/deflate; this backend handles `br` / `zstd` |
| Request CE | Opt-in `:content-encoding` |
| Bodies | `http-protocol` ≥ **0.2.0**: `:form-data` / typed `:data` / `:content` (streams → async) |
| Timeouts | See below. Every phase failure is an `http-timeout-error`. |
| Proxy | `effective-proxy-config` (request `:proxy`, then the client's, then the system default) → `resolve-proxy` for the URL → dexador `:proxy` URL, or NIL for direct. `no_proxy` is honoured. SOCKS5 / `socks5h` go out as `socks5://`. SOCKS4 and SOCKS5 with credentials signal `unsupported-operation`, because dexador has neither: use `http-backend-async`. |
| 4xx/5xx | Returned as `http-response` (httpx style); `:raise-for-status t` to signal |

## Timeouts

`effective-timeout` (request over client; nothing set means 30 s total) is
applied in three parts:

- **connect / read** → dexador `:connect-timeout` / `:read-timeout`, in whole
  seconds. Each falls back to `:total`, then 10 s.
- **total** caps the whole attempt with an SBCL deadline (`sb-sys:with-deadline`),
  body included for a slurped response. A bare number is `:total`, so
  `:timeout 2.0` fails a 5 s response after 2 s on HTTP and HTTPS alike. The
  deadline does not apply off SBCL.
- **read on HTTPS**: dexador puts `:read-timeout` on the socket's fd-stream,
  but cl+ssl's default socket BIO reads the raw descriptor, so that timeout
  never fired on HTTPS (#24). A request with its own read phase (`:read` with
  no `:total`, or one shorter than `:total`) therefore uses cl+ssl's Lisp BIO.
  Its TLS reads then go through the fd-stream, and silence longer than `:read`
  ends the request while a body that keeps arriving is not cut off. The Lisp BIO
  costs throughput: 20 MB over loopback took 120 ms with it and 15–35 ms
  without.

```lisp
(asdf:load-system "http-backend-dexador")
(let ((*http-backend* (http-backend-dexador:make-dexador-backend)))
  (http:get "https://example.com/" :timeout '(:connect 5 :read 10)))
```

## Connection pools

dexador sets a connection's read timeout and BIO only when it opens it, and its
one pool is keyed by `scheme://authority`. A reused connection therefore kept
the timeout of the request that opened it: `:read 1` after a default request
waited out the old 30 s, and a `:timeout 1` request cut later ones short.
The backend keeps one dexador pool per kind of connection and binds
`dexador:*connection-pool*` to it for each call:

- plain HTTP: one pool per read timeout (whole seconds);
- HTTPS over cl+ssl's socket BIO: one pool (that BIO never sees the read
  timeout; the total deadline bounds those requests);
- HTTPS over the Lisp BIO: one pool per read timeout;
- the proxy is part of the key, so a direct request never gets a proxied
  connection.

At most `*max-connection-pools*` (8) pools are kept; making one more closes
the idle connections of the least recently used. `(clear-connection-pools)`
closes every idle connection.

dexador resends a request once on a new connection when a reused one fails,
read timeouts included, and does not look at the method. POST, PATCH and
the other non-idempotent methods (RFC 9110 §9.2.2) therefore never start on
a pooled connection: they open a fresh one, which goes back into the pool
afterwards. An idempotent request on a reused connection can still be sent
twice, so a `:read` timeout there can take up to twice `:read` (`:total`
still caps the call).

## Test

```bash
cl-repo test
```

The suite runs against loopback servers (plain and TLS, with a self-signed test certificate in
`tests/certs/`). It covers mocks of `dex:request`, proxy resolution, and the read / total deadlines.

## Publish

Source publish to GHCR runs from this repo; packaging metadata lives in the `.asd`:

```bash
gh workflow run publish-checkout.yml -R egao1980/http-backend-dexador
```
