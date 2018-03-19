(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
  = fun exp ->
match exp with
| X -> 0
| INT a -> a
| ADD (a,b) -> calculator(a) + calculator (b)
| SUB (a,b) -> calculator(a) - calculator (b)
| MUL (a,b) -> calculator(a) * calculator (b)
| DIV (a,b) -> calculator(a) / calculator (b)
| SIGMA (i,k,ep) ->if calculator(i) = calculator(k) then calculator(eval2(ep,calculator(k))) else  calculator(eval2(ep,calculator(i))) + calculator( SIGMA(ADD(INT 1, i),k ,ep))



and eval2 : exp * int -> exp
=fun (exp,i) ->
match exp with
| X -> INT i
| INT a -> INT a
| ADD (a,b) -> ADD(eval2(a,i),eval2(b,i))
| SUB (a,b) -> SUB(eval2(a,i),eval2(b,i))
| MUL (a,b) -> MUL(eval2(a,i),eval2(b,i))
| DIV (a,b) -> DIV(eval2(a,i),eval2(b,i))


| _ -> raise NotImplemented  (* TODO *)
