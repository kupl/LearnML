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
  = fun (exp, var) ->
		match exp with
		Const(n) -> Const 0
	| Var(v) -> if var = v then Const 1 else Const 0
	|	Power(v,n) ->
			if n > 1 && var = v then
				Times[Const n; Power(v,(n-1))]
			else if n == 1 && var = v then
				Const 1
			else
				Const 0

	| Times(lst)->
		let rec diffTimes : aexp list * string * aexp list * aexp list -> aexp
		= fun (lst, var,sumlst,prelst)->		
			match lst with
				[]->Sum(sumlst)
			|	n::lst2 ->
					let a = Times(prelst @ diff(n,var) :: lst2) in
					diffTimes(lst2, var, sumlst @ a::[], prelst@n::[])
			in
		diffTimes (lst,var,[],[])

	|	Sum(lst)->
		let rec diffSum : aexp list * string * aexp list -> aexp
		= fun (lst,var,sumlst) ->
			match lst with
				[] -> Sum(sumlst)
			|	n::lst2 -> 
					diffSum(lst2,var,sumlst @ diff(n,var)::[])
			in
		diffSum(lst,var,[])
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

  let balanced : mobile -> bool
  = fun mob -> (* raise NotImplemented *)
		let rec cal_weight : mobile -> int
		= fun mob ->
			match mob with
				SimpleBranch(l1,w1), SimpleBranch(l2,w2) -> w1 + w2
			|	CompoundBranch(l1,m1), SimpleBranch(l2,w2) -> cal_weight(m1) + w2
			| SimpleBranch(l1,w1), CompoundBranch(l2,m2) -> w1 + cal_weight(m2)
			| CompoundBranch(l1,m1), CompoundBranch(l2,m2) -> cal_weight(m1) + cal_weight(m2)
		in
		match mob with
			CompoundBranch(l1,m1), CompoundBranch(l2,m2) ->
				if cal_weight(m1) * l1 = cal_weight(m2) * l2 then true else false
		|	CompoundBranch(l1,m1), SimpleBranch(l2,w2) ->
				if cal_weight(m1) * l1 = l2 * w2 then true else false
		| SimpleBranch(l1,w1), CompoundBranch(l2,m2) ->
				if l1 * w1 = cal_weight(m2) * l2 then true else false
		| SimpleBranch(l1,w1), SimpleBranch(l2,w2) ->
				if l1 * w1 = l2 * w2 then true else false
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

  let rec calculator : exp -> int
  = fun exp -> (* raise NotImplemented *)
		match exp with
		| X -> raise NotImplemented
		|	INT(n) -> n
		|	ADD(e1,e2) -> calculator(e1) + calculator(e2)
		| SUB(e1,e2) -> calculator(e1) - calculator(e2)
		| MUL(e1,e2) -> calculator(e1) * calculator(e2)
		| DIV(e1,e2) -> calculator(e1) / calculator(e2)
		| SIGMA(e1,e2,e3) -> 
				let rec sigma : int -> int -> exp -> int
				= fun a b e ->
					let ori = e in
					let rec cal : exp -> int -> exp
					= fun e a->
						match e with
							X -> INT a
						| INT(n) -> INT(n)
						| ADD(e1,e2) -> ADD((cal e1 a), (cal e2 a))
						| SUB(e1,e2) -> SUB((cal e1 a), (cal e2 a))
						| MUL(e1,e2) -> MUL((cal e1 a), (cal e2 a))
						| DIV(e1,e2) -> DIV((cal e1 a), (cal e2 a))
						| SIGMA(e1,e2,e3) -> SIGMA((cal e1 a), (cal e2 a), (cal e3 a))
						in
					if a <= b then sigma (a+1) b ori + calculator(cal e a) else 0
				in
				sigma (calculator e1) (calculator e2) e3
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

  let rec check : exp -> bool
  = fun exp -> (*raise NotImplemented*) (* TODO *)
		let varlst = [] in
		let rec checklst : var list -> var -> bool
		= fun lst var ->
			match lst with
				[] -> false
			|	n::lst2 -> if n = var then true else checklst lst2 var
		in
		let rec checkvar : exp -> var list -> bool
		= fun exp lst ->	
			match exp with
			V(v) -> checklst lst v
		|	P(v,e) -> checkvar e (v::lst)
		| C(e1,e2) -> checkvar e1 lst && checkvar e2 lst
		in
		checkvar exp varlst	
				
end

