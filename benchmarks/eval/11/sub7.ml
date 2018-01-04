type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
| MULT of expr * expr
| DIVIDE of expr * expr
| MAX of expr list

let rec eval expr =
                match expr with
                |NUM a -> a
                |PLUS (NUM a, NUM b) -> a + b
                |PLUS (NUM a, exprr) |PLUS (exprr, NUM a) -> a + (eval exprr)
                |PLUS (exprr, exprrr) -> (eval exprr) + (eval exprrr)

                |MINUS (NUM a, NUM b) -> a - b
                |MINUS (NUM a, exprr) -> a - (eval exprr)
                |MINUS (exprr, NUM a) -> (eval exprr) - a
                |MINUS (exprr, exprrr) -> (eval exprr) - (eval exprrr)


                |DIVIDE (NUM a, NUM b) -> a/b
                |DIVIDE (NUM a, exprr) -> a / (eval exprr)
                |DIVIDE (exprr, NUM a) -> (eval exprr) / a
                |DIVIDE (exprr, exprrr) -> (eval exprr) / (eval exprrr)
                

                |MULT (NUM a, NUM b) -> a*b
                |MULT (NUM a, exprr) |MULT (exprr, NUM a) -> a * (eval exprr)
                |MULT (exprr, exprrr) -> (eval exprr) * (eval exprrr)

                |MAX [] -> 0
                |MAX [NUM a] -> a
                |MAX [NUM a; NUM b] -> max a b
                |MAX [NUM a; exprr] |MAX [exprr; NUM a] -> max a (eval exprr)
                |MAX [exprr; exprrr] -> max (eval exprr) (eval exprrr)
                
                

                |MAX (exprr::[]) -> eval exprr
                |MAX (h1::h2::t)-> max (max(eval h1) (eval h2)) (eval (MAX t))
