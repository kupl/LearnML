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
		| Var x -> if x = var then Const 1 else Var x
		| Power (s, i) -> 
			if s = var then Times [Const i; Power(s, i - 1)] else Const 0
		| Times l -> begin
			match l with 
			| hd::tl -> Sum [Times ((diff (hd, var))::tl);Times[hd;diff((Times tl),var)]]
			| [] -> Const 0
			end
		| Sum l ->
			match l with
			
			| hd::tl -> Sum [diff (hd, var); diff((Sum tl), var)]
			| [] -> Const 0
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
  
  	let rec sum : branch -> int
  	= fun w ->
  	match w with
  	| SimpleBranch(a, b) -> b
  	| CompoundBranch(a, (b, c)) -> (sum b) + (sum c);;

  
  	let rec multiple : branch -> int
  	= fun mobil ->
  	match mobil with
  	| SimpleBranch(a, b) -> a * (sum mobil)
  	| CompoundBranch(a, (b, c)) -> a * (sum mobil);;
  

  	let rec balanced : mobile -> bool
  	= fun mobil ->
  	let rec bal : branch -> bool
  	= fun branc ->
	match branc with
	| SimpleBranch(l, w) -> true
	| CompoundBranch(l, (w1, w2)) -> if (multiple w1) = (multiple w2) then (bal w1) && (bal w2) else false
	in
	match mobil with
	| (a, b) -> ((bal a) && (bal b)) && ((multiple a) = (multiple b))
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

  let rec calculator_inside : exp * int -> int
  = fun (exp, t) -> 
		match exp with
		| X -> t
		| INT n1-> n1
		| ADD (n1, n2) -> (calculator_inside (n1, t)) + (calculator_inside(n2, t))
		| SUB (n1, n2) -> (calculator_inside (n1, t)) - (calculator_inside (n2, t))
		| MUL (n1, n2) -> (calculator_inside (n1, t)) * (calculator_inside (n2, t))
		| DIV (n1, n2) -> (calculator_inside (n1, t)) / (calculator_inside (n2, t))
	 	| SIGMA (i, k, n) ->
			if (calculator_inside (i, t)) > (calculator_inside (k, t)) then 0
			else calculator_inside (n, (calculator_inside (i, t))) + calculator_inside (SIGMA ((INT (calculator_inside (i, t) + 1)), k, n), 0) 

	let calculator : exp -> int
	= fun exp -> calculator_inside (exp, 0)
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

let rec compare : exp -> string list -> bool
  	= fun exp lst ->
    match exp with
    | V x -> begin
    	match lst with
      	| hd::tl -> if x = hd then true else compare exp tl
      	| [] -> false
		end
    | P (x, e1) -> compare e1 (x :: lst)
	| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)

let rec check : exp -> bool
 	= fun exp ->
		let lst = [] in   
		match exp with
		| V x -> false
		| P (x, e1) -> 
			let l = (x :: lst) in compare e1 l
		| C (e1, e2) -> (compare e1 lst) && (compare e2 lst)
end

