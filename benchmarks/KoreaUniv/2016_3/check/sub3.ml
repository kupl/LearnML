(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)
(*)
exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let diff : aexp * string -> aexp
  = fun (exp, var) -> raise NotImplemented (* TODO *)
end
*)
(*********************)
(*     Problem 2     *)
(*********************)

module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec addweight : mobile -> int 
	= fun subadd -> 
	match subadd with
  	| (SimpleBranch(l1,w), CompoundBranch(l2,m)) -> ((w) + addweight m)
  	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> (addweight m1 + addweight m2)
  	| (CompoundBranch(l1, m), SimpleBranch(l2, w)) -> (addweight m + (w))
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> (w1) + (w2)

  let rec balanced : mobile -> bool
  = fun mob -> 
  	match mob with
  	| (SimpleBranch(l1,w), CompoundBranch(l2,m)) -> if (balanced m = false) then false 
  													else if (l1 * w) = (l2 * addweight m) then true
  														else false
  	| (CompoundBranch(l1, m1), CompoundBranch(l2, m2)) -> if (balanced m1 && balanced m2) then
  														if (l1*(addweight m1) = l2*(addweight m2)) then true else false
  													else false
  	| (CompoundBranch(l1, m), SimpleBranch(l2, w)) -> if (balanced m) then 
  													if ((l1*w) = l2*(addweight m)) then true else false
  													else false
  	| (SimpleBranch(l1,w1), SimpleBranch(l2,w2)) -> if (l1 * w1) = (l2 * w2) then true else false

end

(*********************)
(*     Problem 3     *)
(*********************)
exception NONONO

module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec modulation : exp * int -> int
= fun (e,num) ->
match e with
	| X -> num
	| INT ( n ) -> n
	| ADD(e1,e2) -> modulation(e1,num)+modulation(e2,num)
	| SUB(e1,e2) -> modulation(e1,num)-modulation(e2,num)
	| MUL(e1,e2) -> modulation(e1,num)*modulation(e2,num)
	| DIV(e1,e2) -> modulation(e1,num)/modulation(e2,num)
	| SIGMA(e1,e2,e3) -> 
		if modulation(e1,num)>modulation(e2,num) then raise NONONO
		else if modulation(e1,num)=modulation(e2,num) then modulation(e3,modulation(e1,num))
		else modulation(e3,modulation(e1,num))+
		modulation ( SIGMA ( INT(modulation(e1,num)+1) , INT (modulation(e2,num)), e3), num)

  and calculator : exp -> int
  = fun exp -> 
  		match exp with
  		 | X -> raise NONONO
  		 | INT(n) -> n
  		 | ADD(e1,e2) -> calculator(e1) + calculator(e2)
  		 | SUB(e1,e2) -> calculator(e1) - calculator(e2)
  		 | MUL(e1,e2) -> calculator(e1) * calculator(e2)
  		 | DIV(e1,e2) -> calculator(e1) / calculator(e2)
  		 | SIGMA(e1,e2,e3) -> 
  		     if calculator(e1) > calculator(e2) then raise NONONO
      		else if calculator(e1) = calculator(e2) then modulation(e3,calculator(e1))(*plus*)
      		else modulation(e3,calculator(e1)) + calculator(SIGMA(INT (calculator(e1) +1), INT (calculator(e2)), e3))
end

(*********************)
(*     Problem 4     *)
(*********************)

module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec find_match : string * string list -> bool
= fun(vr,vr_list) -> 
	match vr_list with
	| [] -> false
	| hd::tl-> if vr = hd then true else find_match(vr,tl) (*list check*)

let rec vr_match : exp * string list -> bool
= fun(exp,vr_list) -> 
	match exp with 
	|V(vr)-> find_match(vr,vr_list)
	|P(vr,e1)->vr_match(e1,vr::vr_list)
	|C(e1,e2)->if (vr_match(e1,vr_list)&&vr_match(e2,vr_list)) then true else false


  let check : exp -> bool
  = fun exp -> 
  	match exp with
  	| V(vr) -> false
  	| P(vr,e1)-> vr_match(e1,[vr])
   	| C(e1,e2) -> (vr_match(e1,[]) && vr_match(e2,[]))
end
