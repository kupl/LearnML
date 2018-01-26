
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec eval e n =
match e with
| INT n0 -> n0
| X -> n
| ADD (e0, e1) -> (eval e0 n) + (eval e1 n)
| SUB (e0, e1) -> (eval e0 n) - (eval e1 n)
| MUL (e0, e1) -> (eval e0 n) * (eval e1 n)
| DIV (e0, e1) -> (eval e0 n) / (eval e1 n)
| SIGMA (e0,e1,e2) -> 0

let calculator e =
match e with
| SIGMA (e0,e1,e2) -> 
 begin
 let asdfg = ref 0 in
 for i = (eval e0 0) to (eval e1 0) do 
 asdfg := !asdfg + (eval e2 i)
 done;
 !asdfg
 end
| _ -> eval e 0