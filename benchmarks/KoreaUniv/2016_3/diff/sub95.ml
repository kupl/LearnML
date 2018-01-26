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

 let rec map f l =
	match l with
	|[] -> []
	|hd::tl -> (f hd)::(map f tl) 
 
 let rec diff : aexp * string -> aexp
  = fun (exp, var) ->
	match var with
	|str -> 
	(match exp with
	|Const i -> Const 0
	|Var s -> if s = str then Const 1 else Const 0
	|Power (s,i) ->
	 if s = str then (if i = 1 then Const 1
	 else Times [Const i; Power ( s, i-1)]) else Const 0
	|Times l ->
	Times l 
	|Sum l ->
	Sum (map (fun x -> diff(x, str)) l)
	|_-> raise(Failure "NotProper")
	)
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

  let rec calcuWeight : branch * branch -> int
  = fun m ->
       ( match m with
        |(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if((ll * lw) = (rl * rw)) then (lw + rw) else 0
        |(SimpleBranch(ll,lw), CompoundBranch(l,r)) -> if(ll * lw) = (l *(calcuWeight r)) then lw + (l * (calcuWeight r)) else 0
	|(CompoundBranch(l,r),SimpleBranch(rl,rw)) -> if (rl * rw) = (l *(calcuWeight r)) then rw + (l *(calcuWeight r)) else 0
	|(CompoundBranch(ll,lr),CompoundBranch(rl,rr)) -> if(ll *(calcuWeight lr)=(rl *(calcuWeight rr))) then (calcuWeight lr) + (calcuWeight rr) else 0
	|_-> raise(Failure "NotProper")
)

  let balanced : mobile -> bool
  = fun mob -> 
	match mob with 
	|(CompoundBranch(ll,lr),CompoundBranch(rl,rr)) -> if(ll *(calcuWeight lr)=(rl *(calcuWeight rr))) then true else false
	|(SimpleBranch(ll,lw), SimpleBranch(rl,rw)) -> if((ll * lw) = (rl * rw)) then true else false
	|(SimpleBranch(ll,lw), CompoundBranch(l,r)) -> if(ll * lw) = (l *(calcuWeight r)) then true else false
	|(CompoundBranch(l,r),SimpleBranch(rl,rw)) -> if (rl * rw) = (l *(calcuWeight r)) then true else false
	|_-> raise(Failure "NotProper")
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

  let rec getVal : exp * int -> int
  = fun (ep, num) ->
	match ep with
	|X -> num
	|INT i -> i
        |ADD (x, y) -> (getVal (x, num)) + (getVal (y, num))
        |SUB (x, y) -> (getVal (x, num)) - (getVal (y, num))
	|MUL (x, y) -> (getVal (x, num)) * (getVal (y, num))
	|DIV (x, y) -> (getVal (x, num)) / (getVal (y, num))
	|_-> raise (Failure "notProper")

  let calculator : exp -> int
  = fun exp -> 
	match exp with
	|INT i -> i
	|ADD (x, y) -> (getVal (x, 0)) + (getVal (y, 0))
        |SUB (x, y) -> (getVal (x, 0)) - (getVal (y, 0))
        |MUL (x, y) -> (getVal (x, 0)) * (getVal (y, 0))
        |DIV (x, y) -> (getVal (x, 0)) / (getVal (y, 0))
	|SIGMA (f,t,body) -> raise (Failure "NotReadyforSigma")
	|_-> raise (Failure "notProper")	 
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
  = fun exp -> false
end

