type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
  
let rec sigma : int->int->(int->int)->int
=fun n1 n2 f -> if n1=n2 then f n1 else (f n1) + (sigma (n1+1) n2 f);;


let rec calculator : exp->int 
= fun exp ->
let rec exp2fun : exp->(int->int)
=fun exp1 -> match exp1 with
X-> (fun x->x)
|INT n1-> (fun x -> n1)
|ADD (n1,n2)-> (fun x -> (exp2fun n1) x + (exp2fun n2) x)
|SUB (n1,n2)-> (fun x -> (exp2fun n1) x- (exp2fun n2) x)
|MUL (n1,n2)-> (fun x -> (exp2fun n1) x* (exp2fun n2) x)
|DIV (n1,n2)-> (fun x -> (exp2fun n1) x/ (exp2fun n2) x)
|SIGMA (n1,n2,exp2) -> (fun x ->  sigma ((exp2fun n1) x) ((exp2fun n2) x) (exp2fun exp2)) in (exp2fun exp) 0 ;;
