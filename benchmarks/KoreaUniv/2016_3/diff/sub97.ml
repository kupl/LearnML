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
	
	
	let rec length : aexp list -> int 
	=fun lst -> match lst with
	| [] -> 0
	|	hd::tl -> 1 + length tl

  let diff : aexp * string -> aexp
  = fun (exp, var) -> 
  let rec diff_exp : aexp * string -> aexp 
  = fun (exp, var) -> 
  match exp with
  | Const n -> Const 0
  | Var value -> if var=value then Const 1 else Const 0
  | Power (value, n) -> if var=value then Times [Const n; Power (value, n-1)] else Const 0  
	| Sum lst ->
		begin
	  let rec sum_diff : aexp list * string -> aexp list
		= fun(sum_lst, key) ->
		match sum_lst with
		|[] -> []
		|hd::tl -> diff_exp(hd, key)::(sum_diff(tl, key))
		in Sum(sum_diff(lst, var))
		end
	| Times lst ->
		let rec filter : aexp list * string * int * int -> aexp list
		= fun(lst, key, p, q) ->
		match lst with
		| [] -> []
		| hd::tl -> if p==q then diff_exp(hd, key)::filter(tl, key, p+1, q)
			else hd::filter(tl, key, p+1, q)
		in let rec time_diff : aexp list * string * int * int-> aexp list
		= fun(time_lst, key, x, y) -> if x<=y then Times(filter(time_lst, key, 1, x))::time_diff(time_lst, key, x+1, y) else []
		in Sum(time_diff(lst, var, 1, (length lst)))
	in diff_exp(exp, var)

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

	let rec scale : branch -> int
	= fun br -> match br with
	| SimpleBranch(len, w) -> len*w
	| CompoundBranch(len, (x, y)) -> len*(scale(x)+scale(y))

  let balanced : mobile -> bool
  = fun mob -> 
	let rec balance : mobile -> bool
	= fun mob -> match mob with
	|(SimpleBranch(a, b), SimpleBranch(c, d)) -> if a*b = c*d then true else false
	|(CompoundBranch(a, (p ,q)), SimpleBranch(c, d)) -> if a*(scale(p)+scale(q)) = c*d 
		then true&&balance(p, q) else false
	|(SimpleBranch(a, b), CompoundBranch(c, (p, q))) -> if a*b = c*(scale(p)+scale(q)) 
		then true&&balance(p, q) else false
	|(CompoundBranch(a, (x, y)), CompoundBranch(b, (p, q))) -> 
		if a*(scale(x)+scale(y)) = b*(scale(p)+scale(q)) 
		then balance(x, y)&&balance(p, q) else false
	in balance(mob)
	 
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

	let rec trans : exp * int -> exp
  = fun(exp, k) ->
  match exp with
  | X -> INT k
  | INT n -> INT n
  | ADD(a, b) -> ADD(trans(a, k), trans(b ,k))
  | SUB(a, b) -> SUB(trans(a, k), trans(b, k))
  | MUL(a, b) -> MUL(trans(a, k), trans(b, k))
  | DIV(a, b) -> DIV(trans(a, k), trans(b, k))
  | SIGMA(a, b, c) -> trans(c, k)


  let calculator : exp -> int
  = fun exp ->
	let rec calc : exp -> int = fun exp ->
	match exp with 
	| INT n -> n
	| ADD(a, b) -> calc(a) + calc(b)
	| SUB(a, b) -> calc(a) - calc(b)
	| MUL(a, b) -> calc(a) * calc(b)
	| DIV(a, b) -> calc(a) / calc(b)
	| SIGMA(a, b, c) -> if calc(a)>calc(b) then 0 else calc(trans(SIGMA(a, b, c), calc(a))) + calc(SIGMA(ADD(a, INT 1), b, c))
	in calc(exp)

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

	let rec mem : var *  var list -> bool
	= fun (key, lst) -> match lst with
	| [] -> false
	| hd::tl -> if hd = key then true else false||mem(key, tl)

  let check : exp -> bool
  = fun exp ->
	let rec ch : exp * var list-> bool
	= fun(exp, lst) -> match exp with
	| V(x) -> mem(x, lst)
	| P(x, p) -> ch(p, x::lst)
	| C(p, q) -> ch(p, lst)&&ch(q, lst)
	in ch(exp, [])
end
