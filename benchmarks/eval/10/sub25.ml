exception Error of string
exception DividedByZero

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval exp = match exp with
NUM (value) -> value
| PLUS (exp1, exp2) -> ((eval exp1) + (eval exp2))
| MINUS (exp1, exp2) -> ((eval exp1) - (eval exp2))
| MULT (exp1, exp2) -> ((eval exp1) * (eval exp2))
| DIVIDE (exp1, exp2) -> (
                         let dv = eval exp2 in
                         if (dv=0) then (raise DividedByZero)
                         else ((eval exp1) / dv)
                         )
| MAX ([]) -> 0
| MAX (lst) -> List.nth (List.sort (function x -> 
                                                    (function y -> (if (x>y) then 1
                                                    else if(x=y) then 0
                                                    else -1))) 
                                                    (List.map (function x -> eval x) lst)) ((List.length lst)-1) 