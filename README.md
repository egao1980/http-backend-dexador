# http-backend-dexador

MIT. Sync [`http-protocol`](https://github.com/egao1980/http-protocol) backend over [dexador](https://github.com/fukamachi/dexador).

| Concern | Behavior |
|---------|----------|
| `Accept-Encoding` | `default-accept-encoding` (soft-loads encoding backends) |
| Response CE | Auto-decode; dexador already unwraps gzip/deflate — we handle `br`/`zstd` |
| Request CE | Opt-in `:content-encoding` |
| 4xx/5xx | Returned as `http-response` (httpx style); `:raise-for-status t` to signal |

```bash
# siblings: http-protocol/ http-encoding-chipz/ http-backend-dexador/
qlot install
qlot exec ros -S . -e '(asdf:test-system "http-backend-dexador")'
```

```lisp
(asdf:load-system "http-backend-dexador")
(let ((*http-backend* (http-backend-dexador:make-dexador-backend)))
  (http:get "https://example.com/"))
```
