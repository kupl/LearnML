type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let rec calculator : exp -> int
=fun exp -> 
let rec setFlag exp flag num result =
(match exp with
|X -> 
if flag = 0 then raise (Failure "FreeVariable")
else num
|INT a -> a
|ADD (ex1,ex2) -> (setFlag ex1 flag num result) + (setFlag ex2 flag num result)
|SUB (ex1,ex2) -> (setFlag ex1 flag num result) - (setFlag ex2 flag num result)
|MUL (ex1,ex2) -> (setFlag ex1 flag num result) * (setFlag ex2 flag num result)
|DIV (ex1,ex2) -> (setFlag ex1 flag num result) / (setFlag ex2 flag num result)
|SIGMA (ex1,ex2,ex3) ->
let ex1' = setFlag ex1 flag num result in
let ex2' = setFlag ex2 flag num result in 
let num = ex1' in 
let flag = 1 in 
(if ex1' > ex2' then result
else 
let result = result + (setFlag ex3 flag num result) in
setFlag (SIGMA (INT (ex1'+1), INT ex2', ex3)) flag num result)) 
in setFlag exp 0 0 0;;