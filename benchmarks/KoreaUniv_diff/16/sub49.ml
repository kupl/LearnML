
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