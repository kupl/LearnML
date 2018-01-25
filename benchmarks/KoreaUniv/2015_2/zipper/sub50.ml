let rec zipper a b =
match a, b with
| [], _ -> b
| _, [] -> a
| hx :: txs, hy :: tys ->
if hx < hy then hx :: zipper txs b else hy :: zipper a tys
;;
