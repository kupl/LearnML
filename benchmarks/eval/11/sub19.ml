type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr
          | MULT of expr * expr
          | DIVIDE of expr * expr
          | MAX of expr list

let rec eval(expr) =
   match expr with
   | NUM(a) -> a
   | PLUS(a, b) -> eval(a) + eval(b)
   | MINUS(a, b) -> eval(a) - eval(b)
   | MULT(a, b) -> eval(a) * eval(b)
   | DIVIDE(a, b) -> eval(a) / eval(b)
   | MAX a -> if List.length(a) = 0 then 0
     else if List.length(a) = 1 then eval(List.nth a 1)
     else List.nth (List.rev(List.sort compare (List.map eval a))) 1
