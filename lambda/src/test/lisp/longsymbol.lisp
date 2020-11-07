; code that uses a symbol longer than SYMBOL_MAX
; -> too lazy to enter 2k chars

((lambda (a) a)
(quote a\ very\ long\ identifier\ that\ will\ be\ truncated))


; output: (empty)
; result: |a very long identifier that wi|
