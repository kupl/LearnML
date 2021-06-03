(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec length : aexp list -> int 
=fun lst -> match lst with
| [] -> 0
| hd::tl -> 1 + length tl;;

let diff : aexp * string -> aexp
= fun (e,x) -> 
 let rec diff_exp : aexp * string ->aexp
= fun (e,x) ->
match e with
 	| Const n -> Const 0
 	| Var value -> if x=value then Const 1 else Const 0
 	| Power (value, n) -> if x=value then Times [Const n; Power (value, n-1)] else Const 0
 	| Sum lst -> let rec diff_sum : aexp list * string -> aexp list
 				= fun(sum_lst, key) -> match sum_lst with
 										| [] -> []
 										| hd::tl -> diff_exp(hd, key)::(diff_sum(tl, key)) in
 										Sum(diff_sum(lst, x))
 	| Times lst -> let rec f : aexp list*string*int*int -> aexp list
 		= fun (lst, key, p, q) -> match lst with
 									| [] -> []
 									| hd::tl -> if p=q then diff_exp(hd, key)::f(tl, key, (p+1), q)
 												else hd::f(tl, key, (p+1), q) in
 												let rec diff_time : aexp list * string * int * int -> aexp list
 												= fun (time_lst, key, x, y) -> if x>y then [] else Times(f(time_lst, key, 1, x))::diff_time(time_lst, key, (x+1), y)
 											in Sum(diff_time(lst, x, 1, (length lst))) in diff_exp(e, x);;
