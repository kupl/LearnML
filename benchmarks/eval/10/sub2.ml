(* CSE/ 2004-11920 / Yeseong Kim/ Prob 3*)
exception DividedByZero 

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval expr =
        let checkZero z = 
                if (z = 0) then raise DividedByZero
                else z
        in
        let submax l =
                let rec subMax l now =
                        match l with 
                                h::t -> subMax t (max (eval h) now)
                        |       [] -> now
                in
                match l with
                        h::t -> subMax t (eval h)
                |       [] -> 0
        in
        match expr with
                NUM(n) -> n
        |       PLUS(e1, e2) -> eval(e1) + eval(e2)
        |       MINUS(e1, e2) -> eval(e1) - eval(e2)
        |       MULT(e1, e2) -> eval(e1) * eval(e2)
        |       DIVIDE(e1, e2) -> eval(e1) / checkZero(eval(e2))
        |       MAX(l) -> submax l
