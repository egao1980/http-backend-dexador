# http-backend-dexador

MIT. Sync [`http-protocol`](https://github.com/egao1980/http-protocol) backend over [dexador](https://github.com/fukamachi/dexador). **OCI: 0.2.1.**

| Concern | Behavior |
|---------|----------|
| `Accept-Encoding` | `default-accept-encoding`. The encoding backends are soft-loaded once per image through http-protocol's cached probe. |
| Response CE | Auto-decode. Dexador already unwraps gzip/deflate; this backend handles `br` / `zstd` |
| Request CE | Opt-in `:content-encoding` |
| Bodies | `http-protocol` ≥ **0.2.0**: `:form-data` / typed `:data` / `:content` / `:files` reach the origin as http-protocol encoded them (#32). Dexador encodes a body again when its Content-Type is exactly `application/x-www-form-urlencoded` (it takes a non-string for an alist) or starts with `multipart/`. So an urlencoded body goes to it as the string it encodes, and any other such body (multipart, or a coded urlencoded body) keeps its octets, with a leading space in the Content-Type value: optional whitespace before a field value (RFC 9112 §5), which the recipient strips. Multipart and `:content-encoding` bodies are built in memory, as Requests does, because dexador has no stream writer. A stream passed as `:content` signals `unsupported-operation` (use `http-backend-async`). |
| Redirects | Followed by the backend, one dexador request per hop (#33), with http-backend-async's rules. 307/308 resend the method and body. 301/302/303 become a GET (HEAD stays HEAD) without the body or its headers. A redirect to another origin drops `Authorization`, `Proxy-Authorization` and `Cookie` (the cookie jar adds the new origin's). `response-history` lists the redirects, oldest first. More than `:max-redirects` (default 5) signals `http-redirect-error`; `:max-redirects 0` returns the 3xx itself. Before 0.2.1 dexador followed them: the history was empty, the GET after a 303 sent the POST body again, and `Host` lost the port on another origin. |
| Timeouts | See below. Every phase failure is an `http-timeout-error`. |
| Proxy | `effective-proxy-config` (request `:proxy`, then the client's, then the system default) → `resolve-proxy` for the URL → dexador `:proxy` URL, or NIL for direct. Plain HTTP through an HTTP proxy goes out in absolute-form (`GET http://host/path`, RFC 9112 §3.2.2), which needs dexador ≥ 0.9.16.1 (the egao1980/dexador fork; 0.9.16 sent the origin-form and squid answered 400). With an older dexador such a request signals `unsupported-operation` instead. HTTPS goes through `CONNECT`. `no_proxy` is honoured. SOCKS5 / `socks5h` go out as `socks5://`. SOCKS4 and SOCKS5 with credentials signal `unsupported-operation`, because dexador has neither: use `http-backend-async`. So does an `https://` proxy URL (TLS to the proxy): dexador opens plain TCP to every proxy, which would send the request line and `Proxy-Authorization` in cleartext. The error message leaves the proxy credentials out. |
| 4xx/5xx | Returned as `http-response` (httpx style); `:raise-for-status t` to signal |

## Timeouts

`effective-timeout` (request over client; nothing set means 30 s total) is
applied as follows:

- **connect / read** → dexador `:connect-timeout` / `:read-timeout`. Each
  falls back to `:total`, then 10 s. Connect is in whole seconds. Read is exact
  on SBCL off Windows (usocket makes it the fd-stream's timeout, a float), so
  `:read 1.5` waits 1.5 s; elsewhere it rounds up to whole seconds.
- **total** caps the whole attempt with an SBCL deadline (`sb-sys:with-deadline`),
  body included for a slurped response. A bare number is `:total`, so
  `:timeout 2.0` fails a 5 s response after 2 s on HTTP and HTTPS alike. The
  deadline does not apply off SBCL.
- **read on HTTPS**: dexador puts `:read-timeout` on the socket's fd-stream,
  but cl+ssl's default socket BIO reads the raw descriptor, so that timeout
  never fired on HTTPS (#24). A request with its own read phase (`:read` with
  no `:total`, or one shorter than `:total`) therefore uses cl+ssl's Lisp BIO.
  Its TLS reads then go through the fd-stream, and silence longer than `:read`
  ends the request while a body that keeps arriving is not cut off. An
  `http://` URL that follows redirects (the default) uses it too, because its
  redirect to `https://` reads over the same BIO; `:max-redirects 0` keeps the
  socket BIO. The Lisp BIO costs throughput: 20 MB over loopback took 120 ms
  with it and 15–35 ms without.
- **streamed bodies** (`:want-stream t`) are read after `send` returns, so
  `:total` bounds `send` (status line and headers) but not the reads after it.
  Each body read waits at most the read timeout (`:read`, else `:total`, else
  10 s), as with urllib3 and httpx, and as with http-backend-async: a stalled
  body ends, and a body that keeps arriving is not cut off. A streamed HTTPS
  response therefore always reads through the Lisp BIO, and a timeout in a
  body read is an `http-timeout-error`. Before this, HTTPS streams ignored the
  timeout: a body silent for 6 s under `:timeout 2.0` waited it out. A
  streamed body is always binary (`:force-binary t`), because http-protocol
  buffers octets.

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

- plain HTTP: one pool per read timeout and BIO (a redirect to `https://`
  opens TLS in it);
- HTTPS over cl+ssl's socket BIO: one pool (that BIO never sees the read
  timeout; the total deadline bounds those requests);
- HTTPS over the Lisp BIO: one pool per read timeout;
- the proxy is part of the key, so a direct request never gets a proxied
  connection;
- so is `:verify`. The certificate is checked only when a connection opens,
  and the pools belong to the backend and are shared by every client, so a
  `:verify nil` client's connection never serves a client that verifies.

At most `*max-connection-pools*` (8) pools are kept; making one more closes
the idle connections of the least recently used. `(clear-connection-pools)`
closes every idle connection.

A streamed body reads and closes with its own pool bound, so its connection
goes back to that pool when the body ends (before this it went to dexador's
global pool and was never reused). Closed before the end, the connection is
closed instead: dexador would pool it with unread body octets still on the
wire.

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
`tests/certs/`). It covers mocks of `dex:request`, proxy resolution, the read / total deadlines,
request bodies as the origin receives them, and redirects with their history.

## Publish

Source publish to GHCR runs from this repo; packaging metadata lives in the `.asd`:

```bash
gh workflow run publish-checkout.yml -R egao1980/http-backend-dexador
```
