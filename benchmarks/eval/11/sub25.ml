type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval exp =
  let rec get_max(exp_list, max) =
    match exp_list with
    | [] -> max
    | h::t ->(
      if eval(h) > max then
        get_max(t, eval(h))
      else
        get_max(t, max)
    )
  in
  match exp with
  | NUM(x) -> x
  | PLUS(e1, e2) -> eval(e1) + eval(e2)
  | MINUS(e1, e2) -> eval(e1) - eval(e2)
  | MULT(e1, e2) -> eval(e1) * eval(e2)
  | DIVIDE(e1, e2) -> eval(e1) / eval(e2)
  | MAX([]) -> 0
  | MAX(h::t) -> get_max(t, eval(h))
