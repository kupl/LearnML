(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = match l with | [] -> a | hd::tl -> f hd (fold f tl a) 
let rec max : int list -> int
= fun lst -> fold (fun x y -> if x>y then x else y) lst (-9999999) 

let rec min : int list -> int
= fun lst -> fold (fun x y -> if x>y then y else x) lst (9999999) 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with [] -> [] 
													|hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl)

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f(f a) 

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with | Empty -> false 
							| Node(a,node_left,node_right) -> if a=n then true else (mem n node_left) || (mem n node_right)

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
| ZERO -> n2
| SUCC nat_a -> SUCC(natadd nat_a n2)  

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC nat_a -> natadd (natmul nat_a n2) n2 

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

let rec arithmetic = fun f -> match f with 
| Num num -> num
| Plus(num1,num2) -> (arithmetic num1) + (arithmetic num2) 
| Minus(num1,num2) -> (arithmetic num1) - (arithmetic num2)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not fm -> if eval fm then false else true
| AndAlso(fm1,fm2) -> (eval fm1) && (eval fm2)
| OrElse(fm1,fm2) -> (eval fm1) || (eval fm2)
| Imply(fm1,fm2) -> if (eval fm1) then if (eval fm2) then true else false else false 
| Equal(exp1,exp2) -> (arithmetic exp1) = (arithmetic exp2) 

