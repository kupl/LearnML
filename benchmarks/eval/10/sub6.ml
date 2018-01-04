exception DividedByZero of string

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval e =
        match e with
        NUM a -> a
        |PLUS(a,b) -> eval a + eval b
        |MINUS(a,b) -> eval a - eval b
        |MULT(a,b) -> eval a * eval b
        |DIVIDE(a,b) -> if eval b = 0 then raise (DividedByZero ".")
        else eval a / eval b
        |MAX [] -> 0
        |MAX (a::b) -> if(eval a > eval(MAX(b))) then eval a else eval(MAX(b))
;;
