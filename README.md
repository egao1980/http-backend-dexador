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
  ends the request while a body that keeps arriving is not cut off. Such a
  request opens a fresh connection and closes it afterwards: a pooled
  connection keeps the BIO and read timeout it was opened with. The Lisp BIO
  costs throughput: 20 MB over loopback took 120 ms with it and 15–35 ms
  without.

```lisp
(asdf:load-system "http-backend-dexador")
(let ((*http-backend* (http-backend-dexador:make-dexador-backend)))
  (http:get "https://example.com/" :timeout '(:connect 5 :read 10)))
```

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
