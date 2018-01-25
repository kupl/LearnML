(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> (* TODO *)
match lst with
	|[] -> min_int
	|hd::tl -> if hd > max tl then hd else max tl

let rec min : int list -> int
= fun lst -> (* TODO *)
match lst with	
	|[] -> max_int
	|hd::tl -> if hd < min tl then hd else min tl

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = (* TODO *)
match lst with
	|[] -> []
	|hd::tl -> if pred hd then hd::(filter pred tl) else filter pred tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = (* TODO *)
f (f a)
(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
match tree with
| Empty -> false
| Node(m,t1,t2) -> if m=n then true else
					mem n t1 || mem n t2
(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat
 let two = SUCC (SUCC ZERO);; 
 let three = SUCC(SUCC (SUCC ZERO));; 
let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> n2
|SUCC m -> natadd m (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)
match n1 with
|ZERO -> ZERO
|SUCC m -> if m= ZERO then n2 else natadd (natmul n2 m) n2
(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec expfun : exp -> int
= fun q ->
match q with
|Num i -> i
|Plus (i,k) -> expfun i + expfun k
|Minus (i,k) -> expfun i - expfun k


let rec eval : formula -> bool
= fun f -> (* TODO *)
match f with 
|True -> true
|False -> false
|Not f1-> if f1 = True then false else true
|AndAlso(f1,f2) -> if f1 = True && f2 == True then true else false
|OrElse(f1,f2) -> if f1 = False && f2 == False then false else true
|Imply(f1,f2) -> if f1 = True && f2 == False then false else true
|Equal(q1,q2) -> if expfun q1 == expfun q2 then true else false  