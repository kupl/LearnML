(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec test a e
= match e with
| INT n -> n
| X -> a
| ADD(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1+v2))
| SUB(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1-v2))
| MUL(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1*v2))
| DIV(e1,e2) ->
(let v1 = test a e1 in
 let v2 = test a e2 in (v1/v2))
| SIGMA(e1,e2,e3) ->
(let v1 = test a e1 in
 let v2 = test a e2 in
 (if v1 = v2 then test v1 e3
  else ((test v1 e3)+ (test a (SIGMA(INT(v1+1), INT v2, e3))))));;

let rec calculator
= fun e ->
match e with
| INT n -> n
| X -> raise (Failure "Not value")
| ADD(e1,e2) -> 
(match e1, e2 with
 | INT n1, INT n2 -> (n1+n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1+v2))
| SUB(e1,e2) -> 
(match e1, e2 with
 | INT n1, INT n2 ->(n1-n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1-v2))
| MUL(e1,e2) ->
(match e1, e2 with
 | INT n1, INT n2 ->(n1*n2)
 | _,_ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1*v2))
| DIV(e1, e2) ->
(match e1, e2 with
 | INT n1, INT n2 ->(n1/n2)
 | _, _ -> let v1 = calculator e1 in let v2 = calculator e2 in
 (v1/v2))
| SIGMA(e1,e2,e3) ->
let v1 = calculator e1 in
(test v1 e);;