(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

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

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
		|Const n -> Const 0
		| Var t -> if t=var then Const 1 else Const 0
		|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
		|Times lst ->Times ( timediff (lst,var))
		|Sum lst -> Sum (sumdiff (lst,var))
	
	and timediff : aexp list*string -> aexp list
	= fun (lst, var) -> match lst with
		|[]-> [Const 1]
		|hd::tl -> (match hd with
			|Const n->Const n
			|Var t->if t=var then Const 1 else Const 0
			|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
			|Times l2 ->Times ( timediff (l2,var))
			|Sum l2 -> Sum (sumdiff (l2,var)))::(timediff (tl,var))
	
	and sumdiff : aexp list*string -> aexp list
	= fun (lst, var) -> match lst with
		|[]->[Const 0]
		|hd::tl -> (match hd with 
			|Const n->Const 0
			|Var t-> if t=var then Const 1 else Const 0
			|Power (t,n) -> if t=var then Times [Const n; Power (t,n-1)] else Const 0
			|Times l2 -> Times (timediff (l2,var))
			|Sum l2 -> Sum (sumdiff (l2,var)))::(sumdiff (tl,var))
end

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

	let rec we : mobile ->int
	= fun mob -> match mob with
		|(b1,b2)-> 
		begin
			match b1,b2 with
			| ((SimpleBranch (l1,w1)),(SimpleBranch (l2,w2))) -> 
			if l1*w1=l2*w2 then w1+w2 else 0
			| ((SimpleBranch (l1,w1)),(CompoundBranch (l2,m))) ->
			if l1*w1= l2*(we m) then w1+(we m) else 0
			| ((CompoundBranch (l1,m)),(SimpleBranch (l2,w2))) ->
			if l1*(we m)=l2*w2 then (we m)+w2 else 0
			| ((CompoundBranch (l1,m1)),(CompoundBranch (l2,m2))) ->
			if (l1*(we m1)=l2*(we m2) && l1*(we m1)<>0) then (we m1)+(we m2) else 0
		end
  let balanced : mobile -> bool
  = fun mob -> if (we mob)=0 then false else true

end

(*********************)
(*     Problem 3     *)
(*********************)

module Problem3 = struct
	
	type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

	let rec sigma s b f = if b=s-1 then 0 
												else ((sigmacal f b))+sigma s (b-1) f
	
	and sigmacal : exp->int->int
	= fun f a ->  match f with
		|X->a
		|INT n -> n
		|ADD (n1,n2) -> (sigmacal n1 a)+(sigmacal n2 a)
    |SUB (n1,n2) -> (sigmacal n1 a)-(sigmacal n2 a)
    |MUL (n1,n2) -> (sigmacal n1 a)*(sigmacal n2 a)
    |DIV (n1,n2) -> (sigmacal n1 a)/(sigmacal n2 a)
    |SIGMA (n1,n2,f) -> sigma (sigmacal n1 a) (sigmacal n2 a) f
		 
  let rec  calculator : exp -> int
  = fun exp -> match exp with
		|X-> raise (Failure "Input is not correct")
		|INT n -> n
		|ADD (n1,n2) -> (calculator n1)+(calculator n2)
		|SUB (n1,n2) -> (calculator n1)-(calculator n2)
		|MUL (n1,n2) -> (calculator n1)*(calculator n2)
		|DIV (n1,n2) -> (calculator n1)/(calculator n2)
		|SIGMA (n1,n2,f) -> sigma (calculator n1) (calculator n2) f
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

	let rec isthere : string->exp->exp
	= fun key exp -> match exp with
		| V v1 -> if key=v1 then V "true" else V v1
		| P (v1,e1) -> P (v1, isthere key (isthere v1 e1))
		| C (e1,e2) -> C (isthere key e1, isthere key e2)

	let rec nothere : exp -> bool
	= fun exp -> match exp with
		| V v -> if v="true" then true else false
		| P (v,e) -> if (nothere e)=true then true else false
		| C (e1,e2) -> if ((nothere e1)=true && (nothere e2)=true) then true else false

  let check : exp -> bool
  = fun exp -> match exp with
		| V v -> false
		| P (v,e) -> nothere (isthere v e)
		| C (e1,e2) ->  false
end
