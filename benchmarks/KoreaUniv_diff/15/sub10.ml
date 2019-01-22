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

