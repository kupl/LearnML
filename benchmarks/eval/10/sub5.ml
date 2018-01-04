(* 컴퓨터공학부 / 2005-11721 / 김재경 / 숙제2-3 *)
type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr
          | MULT of expr * expr
          | DIVIDE of expr * expr
          | MAX of expr list
exception DividedByZero
let rec eval(expr) =
    match expr with
      NUM(int) -> int
    | PLUS(expr1, expr2) -> eval(expr1) + eval(expr2)
    | MINUS(expr1, expr2) -> eval(expr1) - eval(expr2)
    | MULT(expr1, expr2) -> eval(expr1) * eval(expr2)
    | DIVIDE(expr1, expr2) -> 
        if eval(expr2) = 0 then raise(DividedByZero) else eval(expr1) / eval(expr2)
    | MAX(list) ->
        let rec max_fun(list, max_int) = 
          match list with
            a::list_ex ->
              let max_int = if eval(a) > max_int then eval(a) else max_int in
              max_fun(list_ex, max_int)
          | _ -> max_int in
        max_fun(list, 0)