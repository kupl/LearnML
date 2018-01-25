(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> let a = match lst with
|[]->raise (Failure "list should not be empty")
|hd::tl->hd
in let rec fold f l a = match l with
|[]->a
|hd::tl-> f hd (fold f tl a) in
fold (fun x y -> if x>y then x else y) lst a;;

let rec min : int list -> int
= fun lst -> let a = match lst with
|[]->raise (Failure "list should not be empty")
|hd::tl-> hd 
in let rec fold f l a = match l with
|[]->a
|hd::tl -> f hd (fold f tl a) in
fold (fun x y -> if x<y then x else y) lst a;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
|[]->[]
|hd::tl -> if (pred hd)= true then hd::(filter pred tl) else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
|Empty->false
|Node (a,b1tree,b2tree)-> if a=n then true
										 else if mem n b1tree then true
										 else if mem n b2tree then true else false;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat;;

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with
|ZERO -> n2
|SUCC a -> SUCC(natadd a n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
|ZERO -> ZERO
|SUCC a -> natadd n2 (natmul a n2);;

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
	| Minus of exp * exp;; 

let rec eval : formula -> bool
= fun f -> let rec expfun f = match f with
|Num a -> a
|Plus (a,b) -> (expfun a) + (expfun b)
|Minus (a,b) -> (expfun a) - (expfun b)
in match f with
|True -> true
|False -> false
|Not a -> if eval a = true then false else true 
|AndAlso (a,b) -> if(eval a=true)&&(eval b=true) then true else false
|OrElse (a,b) -> if (eval a=false)&&(eval b=false) then false else true
|Imply (a,b) -> if eval a =false then true
								else if eval b= true then true else false
|Equal (a,b) -> if (expfun a)=(expfun b) then true else false;;
