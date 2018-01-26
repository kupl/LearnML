(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

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
		|Const (i) -> Const (0)
		|Var (s) ->  if s=var then Const (1) else Const (0)
		|Power (s, i) ->if s=var then  Times [Const (i); Power (s, i-1)]
										else Const (0)
		|Times (aexp_list) ->
			let rec times_list_differentiate lst = match lst with
			|[] -> [Const (0)]
			|h::[] -> [diff (h, var)]
			|h::t -> [Times [diff (h, var); Times (t)]; Times [h; diff (Times (t), var)]]
			in
			Sum (times_list_differentiate (aexp_list))
		|Sum (aexp_list) ->
					  let rec sum_list_differentiate lst = match lst with
						|[] -> [Const (0)]
						|h::[] -> [diff (h, var)]
						|h::t -> diff (h, var) :: sum_list_differentiate (t)
						in
						Sum (sum_list_differentiate (aexp_list))
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
  = fun mob ->

	 	let rec weight_of_mobile (mb) = match mb with
			|SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> w1 + w2
			|SimpleBranch (l1, w), CompoundBranch (l2, m) -> w + weight_of_mobile (m)
			|CompoundBranch (l1, m), SimpleBranch (l2, w) -> weight_of_mobile (m) + w
			|CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> weight_of_mobile (m1) + weight_of_mobile (m2)
		in

		match mob with
			|SimpleBranch (l1, w1), SimpleBranch (l2, w2) -> if l1 * w1 = l2 * w2 then true else false
			|SimpleBranch (l1, w), CompoundBranch (l2, m) -> if l1 * w = l2 * weight_of_mobile (m)
				then true && balanced (m) else false
			|CompoundBranch (l1, m), SimpleBranch (l2, w) -> if l1 * weight_of_mobile (m) = l2 * w
				then true && balanced (m) else false
			|CompoundBranch (l1, m1), CompoundBranch (l2, m2) -> if l1 * weight_of_mobile (m1) = l2 * weight_of_mobile (m2)
				then true && balanced (m1) && balanced (m2) else false
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
  = fun exp ->

		let rec function_making a = match a with
			|X -> fun x -> x
			|INT (i) -> fun x -> i
			|ADD (i1, i2) -> fun x -> ((function_making i1) x) + ((function_making i2) x)
			|SUB (i1, i2) -> fun x -> ((function_making i1) x) - ((function_making i2) x)
			|MUL (i1, i2) -> fun x -> ((function_making i1) x) * ((function_making i2) x)
			|DIV (i1, i2) -> fun x -> ((function_making i1) x) / ((function_making i2) x)
		in

		 match exp with
		|X -> raise (Failure "Cannot Produce Int Value")
		|INT (i) -> i
		|ADD (e1, e2) -> calculator (e1) + calculator (e2)
		|SUB (e1, e2) -> calculator (e1) - calculator (e2)
		|MUL (e1, e2) -> calculator (e1) * calculator (e2)
		|DIV (e1, e2) -> calculator (e1) / calculator (e2)
		|SIGMA (e1, e2, e3) -> 
			if calculator (e2) = calculator (e1) then (function_making e3) (calculator e1)
			else if calculator (e2) > calculator (e1)
				then ((function_making e3) (calculator e2)) + calculator (SIGMA (e1, (SUB (e2, INT 1)), e3))
			else raise (Failure "Cannot Produce Int Value")
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
  = fun exp ->

	let rec evaluate expression enviornment = match expression with
		|V v ->
			let rec checker lst var = match lst with
				|[] -> false
				|h::t -> if h = var then true else (checker t var)
			in
			if (checker enviornment v = true) then true else false
		|P (v, e) -> if (evaluate e (enviornment @ [v]) = true) then true else false
		|C (e1, e2) -> if (evaluate e1 enviornment && evaluate e2 enviornment = true) then true else false
	in
	
	evaluate exp []
end

