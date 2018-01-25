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
  = fun (exp, var) -> raise NotImplemented

	let rec diff : aexp * string -> aexp
	= fun(exp, var) ->
		match exp with
		Const n -> Const 0
		| Var x -> if x = var then Const 1 else Const 0
		| Power (a, x) ->
			(if a = var then
				(match x with
				0 -> Const 0
				| 1 -> Const 1
				| 2 -> Times [Const 2 ; Var a]
				| _ -> Times [Const x; Power (a, x-1)])
			else Const 0)
		| Times l->
			(match l with
				[]-> Const 1
				| hd::[] -> diff (hd, var)
				| hd::tl ->
						(match hd with
							Const 0 -> Const 0
							| Const 1 -> diff((Times tl),var)
							| Const b -> Times [Const b; diff((Times tl), var)]
							| _-> (Sum[Times (diff(hd, var)::tl); Times[hd;diff(Times tl, var)]])	
						)
			)
		| Sum l ->
			match l with
				[] -> Const 0
				| hd::[] -> diff(hd, var)
				| hd::tl -> Sum [diff(hd, var); diff(Sum tl, var)];;
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

  let rec balanced : mobile -> bool
  = fun (mob) -> raise NotImplemented (* TODO *)

	let rec weight : branch -> weight
	= fun br -> 
		match br with
		SimpleBranch (l,w) -> w
		|CompoundBranch(l,mo) ->
			(match mo with
				(lb, rb) -> (weight lb) + (weight rb));;

	let rec ratio : branch -> int
	= fun br ->
		match br with
		SimpleBranch(l, w) -> l * w
		|CompoundBranch(l,mo) ->
			(match mo with
				(lb, rb) -> l * (weight lb + weight rb));;

   let rec balanced : mobile -> bool
   = fun (lb, rb)->
			match lb, rb with
				SimpleBranch (l1, w), SimpleBranch(l2, m) ->
					if(ratio lb = ratio rb) then true else false
   		  | SimpleBranch (l1,w),CompoundBranch (l2,m) ->
      		 if (balanced m = true)&&(ratio lb = ratio rb) then true else false
  			| CompoundBranch (l1,m),SimpleBranch (l2,w) ->
      		 if (balanced m = true)&&(ratio lb = ratio rb) then true else false
  			| CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
      		 if (balanced m1 = true) && (balanced m2 = true)&&(ratio rb= ratio lb) then true else false;;

(* 
	let rec branchcheck : mobile ->bool
	= fun(lb,rb) -> false

	let rec branchcheck : mobile ->bool
	= fun (lb, rb)->
		match lb, rb  with
		SimpleBranch (lb1, lb2), SimpleBranch(rb1,rb2) ->
			if (weight lb = weight rb) then true else false
		| SimpleBranch (lb1, lb2), CompoundBranch(rb1, rb2) ->
			if (branchcheck rb = true) && (ratio lb =ratio rb) then true else false
		| CompoundBranch (lb1, lb2), SimpleBranch(rb1, rb2) ->
			if (branchcheck lb = true) && (ratio lb = ratio rb) then true else false
		| CompoundBranch (lb1, lb2), CompoundBranch(rb1, rb2) ->
			if (branchcheck lb = true) && (branchcheck rb = true) && (ratio lb = ratio rb)
				then true else false;;
(*
	let balanced :mobile -> bool
	= fun mob -> check(mob);;
_
	
			SimpleBranch (lb1, lb2), SimpleBranch (rb1, rb2) ->
					if (weight lb  = weight rb) then true else false
			| SimpleBranch (lb1, lb2), CompoundBranch (rb1, rb2) ->
					if(balanced rb = true) && (ratio lb = ratio rb)
																then true else false
			| CompoundBranch (lb1, lb2), SimpleBranch (rb1, rb2) ->
					if(balanced lb = true) && (ratio lb = ratio rb)
																then true else false
			| CompoundBranch (lb1, lb2), SimpleBranch (rb1,rb2) ->
						if(balanced lb = true) && (balanced rb = true) && (ratio lb = ratio rb)
							then true else false;;
			
*)
			
*)
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

  let calculator : exp -> int
  = fun exp -> raise NotImplemented  (* TODO *)
	
	let rec makeequ : exp -> (int -> int)
	= fun exp ->
		match exp with
		| X -> (fun x -> x)
		| INT n -> (fun x -> n)
		| ADD (e1, e2) -> (fun x -> (((makeequ e1) x) + ((makeequ e2) x)))
		| SUB (e1, e2) -> (fun x -> (((makeequ e1) x) - ((makeequ e2) x)))
		| MUL (e1, e2) -> (fun x -> (((makeequ e1) x) * ((makeequ e2) x)))
		| DIV (e1, e2) -> (fun x -> (((makeequ e1) x) / ((makeequ e2) x)))
		| _-> (fun x->0);;


	let rec sigma : int * int * (int -> int) ->int
	= fun (n1, n2, f) ->
		if( n1= n2 ) then f n1
			else (f n1) + sigma(n1+1,n2,f);;

	let rec calculator : exp -> int
	= fun exp ->
		match exp with
			X -> 0
			| INT n -> n
			| ADD (INT n1, INT n2) -> n1 + n2
			| ADD (e1, e2) -> calculator e1 + calculator e2
			| SUB (INT n1, INT n2) -> n1 - n2
			| SUB (e1, e2) -> calculator e1 - calculator e2
			| MUL (INT n1, INT n2) -> n1 * n2
			| MUL (e1, e2) -> calculator e1 * calculator e2
			| DIV (INT n1, INT n2) -> n1 / n2
			| DIV (e1, e2) -> calculator e1 / calculator e2
			| SIGMA (INT n1, INT n2, e) -> sigma (n1, n2, makeequ e)
			| SIGMA (e1, e2, e3) -> sigma(calculator e1, calculator e2, makeequ e3);;
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

  let check : exp -> bool
  = fun exp -> raise NotImplemented (* TODO *)
	
	let rec ch : (var list)*exp->bool
	= fun(l, exp) ->
		match exp with
		V v -> (match l with
								| hd::tl -> if(v=hd) then true else ch(tl, V v))
								| [] -> false
		| P (v, e) -> ch(l::v, e)
		| C (e1, e2) -> if (ch(l, e1) = true) && (ch(l, e2) = true) then true else false;;

	let check : exp -> bool
	= fun exp -> ch([],exp);;
end

