type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun x->
let rec eval_num : exp -> int = fun n->
match n with
| Num n -> n
| Plus (n1,n2) -> eval_num n1 + eval_num n2
| Minus (n1,n2) -> eval_num n1 - eval_num n2
in
match x with
| True -> true
| False -> false
| Not n -> if(eval n = true) then false else true
| AndAlso (n1,n2) -> if(((eval n1)=true)&&((eval n2)=true)) then true else false
| OrElse (n1,n2) -> if(((eval n1)=true)||((eval n2)=true)) then true else false
| Imply (n1,n2) -> if(((eval n1)=true)&&((eval n2)=false)) then false else true
| Equal (n1,n2) -> if ((eval_num n1) = (eval_num n2)) then true else false;;