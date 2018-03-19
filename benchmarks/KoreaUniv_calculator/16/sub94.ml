
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
= fun exp -> match exp with
|INT n->n
|ADD (left,right) -> calculator left + calculator right
|SUB (left,right) -> calculator left - calculator right
|MUL (left,right) -> calculator left * calculator right
|DIV (left,right) -> if calculator right = 0 then raise NotImplemented else calculator left / calculator right
|SIGMA(left,middle,right) -> proc(calculator left,calculator middle, right) 

and  proc2 : int * exp -> int
= fun (a,b) -> match b with
|X -> a
|INT n->n
|ADD (left,right) -> proc2 (a,left) + proc2 (a,right) 
|SUB (left,right) -> proc2 (a,left) - proc2 (a,right)
|MUL (left,right) -> proc2 (a,left) * proc2 (a,right)
|DIV (left,right) -> if proc2 (a,right) = 0 then raise NotImplemented else proc2(a,left) / proc2(a,right) 
|SIGMA (left,middle,right) -> calculator b 

and  proc : int*int*exp -> int
= fun (a,b,c) -> if a<=b then proc2(a,c) + proc(a+1,b,c) else 0
