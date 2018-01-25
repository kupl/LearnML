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
		| Const n -> Const 0
		| Var x -> if x = var then Const 1 else Const 0
		| Power (x, n) ->
			if x = var then
				begin
				if n = 1 then Const n
				else Times [Const n; Power (x, n-1)]
				end
			else Const 0
		| Times lst ->
			let rec length l =
				begin
				match l with
				| [] -> 0
				| hd::tl -> 1 + length tl
				end
				in
				let rec check1 : aexp list -> int -> aexp list
				= fun lst' n ->
					begin
					match lst' with
					| [] -> []
					| hd::tl -> if n = 0 then [diff (hd, var)]@tl else hd::(check1 tl (n-1))
					end
				in
				let rec check2 : aexp list -> int -> aexp list
				= fun lst' n -> if n = (length lst' - 1) then [Times (check1 lst' n)] else [Times (check1 lst' n)]@(check2 lst' (n+1))
					in Sum (check2 lst 0)
		| Sum lst ->
			let rec check : aexp list -> aexp list
			= fun lst' ->
				begin
				match lst' with
				| [] -> []
				| hd::[] -> [diff (hd, var)]
				| hd::tl -> [(diff (hd, var))]@(check tl)
				end
			in Sum (check lst)
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
		let rec get_weight : branch -> int
		= fun brnch ->
		begin
		match brnch with
		| SimpleBranch (l, w) -> w
		| CompoundBranch (l, (b1, b2)) -> get_weight b1 + get_weight b2
		end in
			begin
			let (b1, b2) = mob in
			match mob with
			| (SimpleBranch (l1, w1), SimpleBranch (l2, w2)) -> if l1*w1 = l2*w2 then true else false
			| (SimpleBranch (l1, w1), CompoundBranch (l2, m2)) -> if (balanced m2) && l1*w1 = l2*(get_weight b2) then true else false
			| (CompoundBranch (l1, m1), SimpleBranch (l2, w2)) -> if (balanced m1) && l1*(get_weight b1) = l2*w2 then true else false
			| (CompoundBranch (l1, m1), CompoundBranch (l2, m2)) -> if (balanced m1) && (balanced m2) && l1*(get_weight b1) = l2*(get_weight b2) then true else false
			end
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
		match exp with
		| X -> 0
		| INT n -> n
		| ADD (e1, e2) -> (calculator e1) + (calculator e2)
		| SUB (e1, e2) -> (calculator e1) - (calculator e2)
		| MUL (e1, e2) -> (calculator e1) * (calculator e2)
		| DIV (e1, e2) -> (calculator e1) / (calculator e2)
		| SIGMA (start, finish, e) ->
				let rec eval_sigma : exp -> int -> int
				= fun exp' i ->
					match exp' with
					| X -> i
					| INT n -> n
					| ADD (e1, e2) -> (eval_sigma e1 i) + (eval_sigma e2 i)
					| SUB (e1, e2) -> (eval_sigma e1 i) - (eval_sigma e2 i)
					| MUL (e1, e2) -> (eval_sigma e1 i) * (eval_sigma e2 i)
					| DIV (e1, e2) -> (eval_sigma e1 i) / (eval_sigma e2 i)
					| SIGMA (_, _, _) -> calculator (exp')
				in
				let rec sigma : int -> int -> exp -> int
				= fun start' finish' exp' ->
					if start' > finish' then 0
					else (eval_sigma exp' start') + (sigma (start'+1) finish' exp')
				in sigma (calculator start) (calculator finish) e
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
		let rec make_list : exp -> var list * var list -> var list * var list
		= fun exp' (lst1, lst2) ->
			match exp' with
			| V v -> (lst1, v::lst2)
			| P (v, e) -> make_list e (v::lst1, lst2)
			| C (e1, e2) -> make_list e1 (make_list e2 (lst1, lst2))
		in
		let rec exist : var list -> var -> bool
		= fun lst v ->
			match lst with
			| [] -> false
			| hd::tl -> if hd = v then true else exist tl v
		in
		let rec real_check : var list * var list -> bool
		= fun (lst1, lst2) ->
			match lst2 with
			| [] -> true
			| hd::tl -> if exist lst1 hd then real_check (lst1, tl) else false
		in real_check (make_list exp ([], []))
end
