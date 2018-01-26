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
	Const (a) -> Const 0
	| Var (s) -> if s = var then (Const 1) else Const 0
	| Power (s, a) -> if s = var then (if a = 1 then Const 1 else Times [Const a; Power (s, a-1)]) else Const 0
	| Times t
	-> (match t with [] -> Const 0
	| h::t -> if t = [] then Sum [Times [(diff (h, var)); Const 1]; Times [h;(diff (Times t, var))]]
	else Sum [Times [(diff (h, var)); Times t]; Times [h;(diff (Times t, var))]]) | Sum t2 -> match t2 with [] -> Const 0 | h::t -> Sum [(diff (h, var));(diff (Sum t, var))];;
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
  let rec g br = match br with SimpleBranch (l, w) -> w | CompoundBranch (l, m) -> (match m with (a, b) -> (g a) + (g b));;
  let rec f br = match br with SimpleBranch (l, w) -> l*w | CompoundBranch (l, m) -> (match m with (a, b) -> if (f a)=(f b) then (((g a)+(g b))*l) else (-1));;
  let balanced : mobile -> bool
  = fun (lb, rb) -> if ((f lb) = (f rb)) then true else false;;
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
  = fun exp ->	let rec f t = (match t with X -> raise (Failure "impossible") |
				INT n1 -> n1 |
				ADD (n1, n2) -> (f n1) + (f n2) |
				SUB (n1, n2) -> (f n1) - (f n2) |
				MUL (n1, n2) -> (f n1) * (f n2) |
				DIV (n1, n2) -> (f n1) / (f n2) |
				SIGMA (n1, n2, n3) ->
					let rec cal k = (match k with X -> (fun x -> x) |
					INT i -> (fun x->i) |
					ADD (p, q) -> (fun x -> ((cal p) x)+((cal q) x)) |
					SUB (p, q) -> (fun x -> ((cal p) x)-((cal q) x)) |
					MUL (p, q) -> (fun x -> ((cal p) x)*((cal q) x)) |
					DIV (p, q) -> (fun x -> ((cal p) x)/((cal q) x)) |
					SIGMA (p, q, r) -> raise (Failure "impossible")) in
			let rec g v1 v2 = if v1 > v2 then 0 else if v1=v2 then ((cal n3) v1) else (((cal n3) v1) + (g (v1 + 1) v2)) in (g (f n1) (f n2))) in
			match exp with X -> 0 | 
			INT a -> a |
			ADD (a, b) -> (f (ADD (a, b))) |
			SUB (a, b) -> (f (SUB (a, b))) |
			MUL (a, b) -> (f (MUL (a, b))) |
			DIV (a, b) -> (f (DIV (a, b))) |
			SIGMA (a, b, c) -> (f (SIGMA (a, b, c)));;
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
  let rec g e s =
		match e with
		V a -> let rec f a b = match a with [] -> false | h::t -> if h = b then true else (f t b) in if (f s a) then true else false |
		P (v, e1) -> if (g e1 (s@[v])) then true else false |
		C (e1, e2) -> if (g e1 s && g e2 s) then true else false;;
  let check : exp -> bool
  = fun exp -> g exp [];;
end