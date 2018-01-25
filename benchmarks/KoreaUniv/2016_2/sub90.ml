(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> let rec fold f l = match l with | a::[]->a | hd::tl -> f hd (fold f tl) in fold(fun x y -> if x>=y then x else y) lst;;

let rec min : int list -> int
= fun lst -> let rec fold f l = match l with | a::[]->a | hd::tl -> f hd (fold f tl) in fold(fun x y -> if x<=y then x else y) lst;;


(*********************)
(*     Problem 2     *)
(*********************)

let rec filter pred lst =match lst with |[]->[]| hd::tl -> if pred hd then hd::(filter pred tl)else []@(filter pred tl);; 

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
              | Empty -> false
              | Node(a,Empty,Empty)-> if a==n then true else false
              | Node(a, b, c) -> if a==n then true else (mem n b)||(mem n c);;

(*********************)
(*     Problem 5     *)
(*********************)

type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
             | ZERO -> n2
	     | SUCC ZERO -> SUCC n2
             | SUCC a -> SUCC (natadd a n2)  
;; 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with
             | ZERO -> ZERO
   	     | SUCC ZERO -> n2
	     | SUCC a -> natadd n2 (natmul a n2)
;;

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
	| True -> true
	| False -> false
	| Not a -> if (eval a) then false else true
	| AndAlso (a,b) -> if (eval a)&&(eval b) then true else false
	| OrElse (a,b) -> if (eval a)||(eval b) then true else false
	| Imply (a,b) -> if not (eval a)||(eval b) then true else false
	|  Equal (a,b) -> let rec exp z = match z with
                       			| Num n-> n
                			| Plus(x,y) -> exp(x)+exp(y)
					| Minus(x,y) -> exp(x)-exp(y)
            in if (exp a)==(exp b) then true else false
;;





