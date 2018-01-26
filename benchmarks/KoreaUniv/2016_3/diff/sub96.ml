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
	| Const a -> Const 0
	| Var a -> if a = var then Const 1 else Const 0
	| Power (a, b) -> if a <> var || b = 0 then Const 0 else if b = 1 then Const 1 else if b = 2 then Times [Const b; Var a] else Times [Const b; Power (a, b - 1)]
	| Times a -> (match a with
		| [] -> raise NotImplemented
		| [hd] -> diff (hd, var)
		| hd::tl -> if diff (hd, var) = Const 0 then Times ([hd]@[diff (Times tl, var)]) else Sum [Times ([diff(hd, var)]@tl); Times ([hd]@[diff (Times tl, var)])])
	| Sum a -> (match a with
		| [] -> raise NotImplemented
		| [hd] -> diff (hd, var)
		| hd::tl -> if diff (hd, var) = Const 0 then diff (Sum tl, var) else (match diff(Sum tl, var) with
			| Sum b -> Sum ([diff(hd, var)]@b)
			| Const 0 -> Sum ([diff (hd, var)])
			| _ -> Sum ([diff (hd, var)]@[diff(Sum tl, var)])))
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
  = fun mob -> let rec wei x = match x with
  | (SimpleBranch(i, j), SimpleBranch(k, l)) -> j + l
  | (SimpleBranch(i, j), CompoundBranch(k, l)) -> j + wei l
  | (CompoundBranch(i, j), SimpleBranch(k, l)) -> wei j + l
  | (CompoundBranch(i, j), CompoundBranch(k, l)) -> wei j + wei l
  in match mob with
  | (SimpleBranch(a, b), SimpleBranch(c, d)) -> if a <= 0 || b <= 0 || c <= 0 || d <= 0 then raise NotImplemented else a * b = c * d
  | (SimpleBranch(a, b), CompoundBranch(c, d)) -> if a <= 0 || b <= 0 then raise NotImplemented else a * b = c * wei d
  | (CompoundBranch(a, b), SimpleBranch(c, d)) -> if c <= 0 || d <= 0 then raise NotImplemented else a * wei b = c * d
  | (CompoundBranch(a, b), CompoundBranch(c, d)) -> a * wei b = c * wei d
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
  = fun exp -> match exp with
  | INT a -> a
  | ADD (a, b) -> calculator(SIGMA(INT 1, INT 1, ADD(a, b)))
  | SUB (a, b) -> calculator(SIGMA(INT 1, INT 1, SUB(a, b)))
  | MUL (a, b) -> calculator(SIGMA(INT 1, INT 1, MUL(a, b)))
  | DIV (a, b) -> calculator(SIGMA(INT 1, INT 1, DIV(a, b)))
  | SIGMA (a, b, c) -> if calculator(a) > calculator(b) then raise NotImplemented
                       else if calculator(a) = calculator(b) then (match c with
		| ADD (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) + calculator(a)
			| (X, f) -> calculator(a) + calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) + calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) + calculator(SIGMA(a, b, g)))
		| SUB (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) - calculator(a)
			| (X, f) -> calculator(a) - calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) - calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) - calculator(SIGMA(a, b, g)))
		| MUL (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) * calculator(a)
			| (X, f) -> calculator(a) * calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) * calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) * calculator(SIGMA(a, b, g)))
		| DIV (d, e) -> (match (d, e) with
			| (X, X) -> calculator(a) / calculator(a)
			|	(X, f) -> calculator(a) / calculator(SIGMA(a, b, f))
			| (f, X) -> calculator(SIGMA(a, b, f)) / calculator(a)
			| (f, g) -> calculator(SIGMA(a, b, f)) / calculator(SIGMA(a, b, g)))
		| INT a -> a)
                       else calculator(SIGMA(a, a, c)) + calculator(SIGMA(INT (calculator(a) + 1), b, c))
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
  = fun exp -> match exp with
	| V a -> false
	| P(a, b) -> (match exp with
		| V c -> if a = c then true else false
		| P(c,d) -> check (P(c,d)) || check (P(a,d))
		| C(c,d) -> check (P(a,c)) && check (P(a,d)))
	| C(a,b) -> (match (a, b) with
		|	(P(c,d), P(e,f)) -> check (P(c,d)) && check (P(e,f))
		| _ -> false)
end

