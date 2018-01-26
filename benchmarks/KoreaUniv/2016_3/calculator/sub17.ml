(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec replace(e1,e3) = 
 match e3 with
 | X -> e1
 | INT(a) -> INT(a)
 | ADD(ea,eb) -> ADD(replace(e1,ea),replace(e1,eb))
 | SUB(ea,eb) -> SUB(replace(e1,ea),replace(e1,eb))
 | MUL(ea,eb) -> MUL(replace(e1,ea),replace(e1,eb))
 | DIV(ea,eb) -> DIV(replace(e1,ea),replace(e1,eb))
 | SIGMA(ea,eb,ec) -> SIGMA(replace(e1,ea),replace(e1,eb),replace(e1,ec));;

let rec calculator exp = 
 match exp with
 | INT(a) -> a
 | ADD(e1,e2) -> calculator(e1)+calculator(e2)
 | SUB(e1,e2) -> calculator(e1)-calculator(e2)
 | MUL(e1,e2) -> calculator(e1)*calculator(e2)
 | DIV(e1,e2) -> calculator(e1)/calculator(e2)
 | SIGMA(e1,e2,e3) -> 
   if calculator(e1)=calculator(e2) then calculator(replace(e1,e3))
   else calculator(ADD(replace(e1,e3),SIGMA(ADD(e1,INT(1)),e2,e3)));;