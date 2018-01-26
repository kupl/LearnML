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

let rec sumiter : aexp list -> string -> aexp list -> aexp list
= fun lst var ans ->
match lst with
[] -> ((Sum ans)::[])
|h::t -> sumiter t var (ans@(rdiff h var []))	
and timesiter : aexp list -> string -> aexp list -> aexp list -> aexp list
= fun lst var headlist ans ->
match lst with
[] -> ((Sum ans)::[])
|h::t -> timesiter t var (h::headlist) ((Times (headlist@(rdiff h var [])@t))::ans)
and rdiff : aexp -> string -> aexp list -> aexp list
 = fun exp var ans ->
	match exp with
	Const(a) -> (Const 0)::ans
	|Var(a) -> if a = var then (Const 1)::ans else (Const 0)::ans
	|Power(a, b) -> if a = var && b>0 then (Times ((Const b)::(Power (a, b-1))::[]))::ans else (Const 0)::ans;
	|Times(lst) -> ans@(timesiter lst var [] [])
	|Sum(lst) -> ans@(sumiter lst var []);;

  let diff : aexp * string -> aexp
  = fun (exp, var) -> 
	let anslist = [] in
	Sum (rdiff exp var anslist );;
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

	let fst p = match p with (x,_) -> x;;
	let snd p = match p with (_,x) -> x;;

	
let rec exp : mobile -> int
	= fun mob ->
	match fst mob with
	SimpleBranch (l, w) ->
		( let left =   w in
	  match snd mob with
			SimpleBranch (l, w) -> left + w
			|CompoundBranch (l, m) -> left + exp m
		)
  | CompoundBranch (l, m) ->
		( let left = exp m
		in match snd mob with
			SimpleBranch (l, w) -> left + w
			|CompoundBranch (l, m) -> left + exp m );;
	 

  let rec balanced : mobile -> bool
  = fun mob ->
	match fst mob with
	SimpleBranch(l, w) -> 
		(let left = l*w in
			match snd mob with
			SimpleBranch(l, w) -> if left = l*w then true else false
			|CompoundBranch(l, m) ->
												if balanced m = true then 
														if left = l*exp m then true else false 
												else false)
	|CompoundBranch(l, m) ->
		(if balanced m = true then
				(let left = l*exp m in
				match snd mob with
				SimpleBranch(l, w) -> if left = l*w then true else false
				|CompoundBranch(l, m) ->
												if balanced m = true then
													if left = l*exp m then true else false
												else false )
		else false);; 



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

  let rec xcal : exp -> int -> int
  = fun exp x ->
	match exp with
	X -> x
	|INT(a) -> a
	|ADD(a, b) -> (xcal a x) + (xcal b x)
	|SUB(a, b) -> (xcal a x) - (xcal b x)
	|MUL(a, b) -> (xcal a x) * (xcal b x)
	|DIV(a, b) -> (xcal a x) / (xcal b x) 
	|_ -> 0;;

let rec sigma : int -> int -> exp -> int
= fun s e exp ->
	if s = e then xcal exp s else xcal exp s + sigma (s+1) e exp;;

  let rec calculator : exp -> int
  = fun exp -> 
	match exp with
	INT(a) -> a
	|ADD(a, b) -> (calculator a) + (calculator b)
	|SUB(a, b) -> (calculator a) - (calculator b)
	|MUL(a, b) -> (calculator a) * (calculator b)
	|DIV(a, b) -> (calculator a) / (calculator b)
	|SIGMA(a, b, c) -> sigma (calculator a) (calculator b) c
	|_ -> 0;;

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

  let rec scan : string list -> string -> bool
  = fun lst s ->
	match lst with
	[] -> false
	|h::t -> if h = s then true else scan t s;;

  let rec rcheck : exp -> string list -> bool
  = fun exp stack -> 
	match exp with 
	V(a) -> scan stack a
	|P(a, b) -> rcheck b (a::stack)
	|C(a, b) -> rcheck a stack  && rcheck b stack;;

let check : exp -> bool
= fun exp ->
	let stack = []
	in rcheck exp stack;; 
end

