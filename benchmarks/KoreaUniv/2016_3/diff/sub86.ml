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
			|Const a -> Const 0
			|Var x -> if x = var then Const 1 else Var x
			|Power (s, i) ->
				if s = var then Times[Const i; Power (s, i-1)] else Power(s, i)
			|Times l -> begin
				match l with
				|[] -> Const 1
				|[e]-> diff (e, var)
				|hd::tl -> Sum( Times (diff(hd, var)::tl) :: Times(hd::diff(Times(tl), var)::[])::[]) end
			|Sum l ->
				match l with
				|[] -> Const 0
				|[e]-> diff (e, var)
				|hd::tl -> Sum(diff(hd, var)::diff(Sum(tl), var)::[])
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

	let rec weight : mobile -> int
	= fun n ->
		match n with
		|(SimpleBranch(_, a), SimpleBranch(_, b)) -> a + b
		|(SimpleBranch(_, a), CompoundBranch(_, b)) -> a + weight(b)
		|(CompoundBranch(_, a), SimpleBranch(_, b)) -> weight(a) + b
		|(CompoundBranch(_, a), CompoundBranch(_, b)) -> weight(a) + weight(b)

  let balanced : mobile -> bool
  = fun mob ->
		match mob with
		|(SimpleBranch(a, b), SimpleBranch(c, d)) ->
			if a*b = c*d then true else false
		|(SimpleBranch(a, b), CompoundBranch(c, d)) ->
			if a*b = c*weight(d) then true else false
		|(CompoundBranch(a, b), SimpleBranch(c, d)) ->
			if a*weight(b) = c*d then true else false
		|(CompoundBranch(a, b), CompoundBranch(c, d)) ->
			if a*weight(b) = c*weight(d) then true else false
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

  let rec eval : exp * exp -> int
	= fun (exp, n) ->
		match exp with
		|X -> eval(n, n)
		|INT a -> a
		|ADD (a ,b) -> eval(a, n) + eval(b, n)
		|SUB (a, b) -> eval(a, n) - eval(b, n)
		|MUL (a, b) -> eval(a, n) * eval(b, n)
		|DIV (a, b) -> eval(a, n) / eval(b, n)
		|SIGMA (a, b, f) -> if eval(a, b) = eval(b, a) then eval(f, a)
												else eval(SIGMA (ADD(a, INT 1), b, f), INT 0)
														 + eval(f, a)

  let rec calculator : exp -> int
  = fun exp -> eval (exp, X)
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

	let rec save : exp -> var list
	= fun exp ->
	match exp with
	|V a -> []
	|P (a, b) -> a :: []
	|C (a, b) -> save (a) @ save(b)

	let rec search : exp * var list -> bool
	= fun (exp, all) ->
	match exp  with
	V a -> begin match all with [] -> false |hd::tl -> if hd =  a then true else search (V a, tl) end
	|P(a, b) -> search (b, (a::all))
	|C(a, b) -> search (a, all) && search (b, all)

  let rec check : exp -> bool
  = fun exp ->
	match exp with
	|V a -> false
	|P (a, b) -> search(b, a::[])
	|C (a, b) -> check (a) && check (b)
end

