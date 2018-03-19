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

