(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = match l with 
|[]->a
|hd::tl-> f hd (fold f tl a);;


let rec max : int list -> int
= fun lst ->match lst with
	|[] -> 0
	|hd::tl ->  fold (fun x y -> if x>y then x else if x=y then x else y ) lst hd;; 

let rec min : int list -> int
= fun lst ->match lst with
|[]->0
|hd::tl->  fold (fun x y -> if x<y then x else if x=y then x else y ) lst hd;; 

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =  match lst with
| [] -> []
| hd :: tl -> if pred hd then hd :: ( filter pred tl ) 
else ( filter pred tl ) ;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty->false
| Node (y, left, right) -> n = y || mem n left || mem n right;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
|ZERO-> n2
|SUCC(n1_1)-> SUCC(natadd n1_1 n2) ;;
 
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
|ZERO->ZERO
|SUCC(n1_1)-> natadd n2(natmul n1_1 n2) ;;
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

let rec eval : formula -> bool
= fun f -> match f with
|True -> true
|False ->false
|Not(x) ->not(eval x)
|AndAlso(x,y) -> (eval x) && (eval y)
|OrElse(x,y)-> (eval x)||(eval y)
|Imply(x,y) -> not (eval x) || (eval y)
|Equal(x,y)-> let rec equality e=match e with 
		|Num(a)->a
		|Plus(a,b)->equality(a)+equality(b)
		|Minus(a,b)->equality(a)-equality(b)
		in if equality(x)=equality(y) then true else false
;;		
