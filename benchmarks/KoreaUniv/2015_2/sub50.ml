let rec filter p = function
| [] -> []
| hd :: tl when p hd -> hd :: filter p tl
| _ :: tl -> filter p tl
;;

let rec zipper a b =
match a, b with
| [], _ -> b
| _, [] -> a
| hx :: txs, hy :: tys ->
if hx < hy then hx :: zipper txs b else hy :: zipper a tys
;;

let rec iter n f x =
if n = 0 then x
else iter (n-1) f (f x)
;;

type expr= X
| Int of int
| Sum of expr * expr
| Sub of expr * expr
| Mul of expr * expr
| Div of expr * expr
| Sigma of expr * expr * expr
;;
let rec eval_expr = function
| Int x -> x
| Sum(e1, e2,Sigma) -> (eval_expr (e1,Sigma)) + (eval_expr (e2,Sigma))
| Sub(e1, e2,Sigma) -> (eval_expr (e1,Sigma)) - (eval_expr (e2,Sigma))
| Mul(e1, e2,Sigma) -> (eval_expr (e1,Sigma)) * (eval_expr (e2,Sigma))
| Div(e1, e2,Sigma) -> (eval_expr (e1,Sigma)) / (eval_expr (e2,Sigma))

