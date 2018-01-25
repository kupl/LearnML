(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = 
	match lst with
		[]->[]
		|hd::tl->if pred hd then hd::(filter pred tl) else (filter pred tl);;
		

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match b with
		[]->a
		|hd::tl->(match a with
				[]->b
				|h::t->h::hd::zipper (t,tl));;



(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) x->
	match n with
		0->x
		|_->f (iter (n-1,f) x);;




		
(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp =  
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> aexp (* TODO *)
let rec diff (aexp,x)=
	match aexp with
		Const n ->Const 0
		|Var y ->if x=y then Const 1 else Const 0
		|Power (y1,n)->if y1=x then Times[Const n;Power(x,n-1)] else Const 0
		|Times (l1)->(match l1 with 
				[]->Const 0
				|hd::tl->Sum ([             Times (diff (hd,x):: tl) ; Times(hd::[diff (Times tl,x)])           ]))
		|Sum (l)->(match l with
				[]->Const 0
				|hd::tl->Sum ([diff (hd,x); diff (Sum (tl),x)]));;


(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e -> 0 (* TODO *)

let rec change :exp->int
=fun e ->0


let rec expchange (x,cexp)=
	match x with 
				X->cexp
				|INT n->INT n
				|ADD(X,X)->ADD(cexp,cexp)
				|ADD(ee,X)|ADD(X,ee)->ADD(ee,cexp)
				|SUB(X,X)->SUB(cexp,cexp)
				|SUB(ee,X)->SUB(ee,cexp)
				|SUB(X,ee)->SUB(cexp,ee)
				|MUL(X,X)->MUL(cexp,cexp)
				|MUL(ee,X)|MUL(X,ee)->MUL(ee,cexp)
				|DIV(X,X)->DIV(cexp,cexp)
				|DIV(ee,X)->DIV(ee,cexp)
				|DIV(X,ee)->DIV(cexp,ee) 
				|ADD(x1,x2)->ADD(expchange(x1,cexp),expchange(x2,cexp))
				|SUB(x1,x2)->SUB(expchange(x1,cexp),expchange(x2,cexp))
				|MUL(x1,x2)->MUL(expchange(x1,cexp),expchange(x2,cexp))
				|DIV(x1,x2)->DIV(expchange(x1,cexp),expchange(x2,cexp))	
				
let rec change e=
	match e with
		INT n->n
		|ADD(e1,e2)->(change e1)+(change e2)
		|SUB(e1,e2)->(change e1)-(change e2)
		|MUL(e1,e2)->(change e1)*(change e2)
		|DIV(e1,e2)->if (change e2)=0 then raise(Failure "cannot not devide by 0") else (change e1)/(change e2)
		|SIGMA (e1,e2,e3)->if (change e1)>(change e2) then 0 else change(expchange (e3,e1))+change(SIGMA ((INT((change e1)+1),e2,e3)))
					
		    
	 	 			 	 	 	 	 				


let calculator exp=
	match exp with
		X->raise(Failure"no variable")
		|INT n->n
		|ADD(exp1,exp2)->change (ADD(exp1,exp2))
		|SUB(exp1,exp2)->change (SUB(exp1,exp2))
		|MUL(exp1,exp2)->change (MUL(exp1,exp2))
		|DIV(exp1,exp2)->change (DIV(exp1,exp2))
		|SIGMA(exp1,exp2,exp3)->change (SIGMA(exp1,exp2,exp3))

