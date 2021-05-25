type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec findName : (var * (var list)) -> bool = fun (x,y) ->
 match y with
 | [] -> false
 | y_h::y_t ->
  if (y_h = x) then true
  else findName (x, y_t)

let rec checkList : (lambda * (var list)) -> bool = fun (x,y) ->
 match x with
 | V n -> findName (n, y)
 | P (n, m) -> checkList (m, (n::y))
 | C (m1, m2) -> ((checkList (m1, y)) && (checkList (m2, y)))
 
let check : lambda -> bool = fun (x) -> checkList(x, [])
