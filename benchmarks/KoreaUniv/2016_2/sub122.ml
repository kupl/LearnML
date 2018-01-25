(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

let who_is_G x y = if x>y then x else y;;
let who_is_S x y = if x>y then y else x;;

let rec fold f l a =
	match l with
	| []->1
	| hd::tl -> f hd (fold f tl a);;

let max lst = fold(who_is_G) lst 1;;
let min lst = fold(who_is_S) lst 1;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = [] (* TODO *)

let rec filter pred lst = 
	match lst with 
		|[]->[] 
		|h::t -> if pred(h) then h::(filter pred t) else (filter pred t);; 


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a (* TODO *)

let inc x = x+1;;
let mul x = x*2;;
let double f a = f(f(a));;


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true (* TODO *)


let t1 = Node(1, Empty, Empty);;
let t2 = Node(1, Node(2,Empty,Empty), Node(3,Empty,Empty));;

let rec mem n tree = 
	match tree with 
		|Empty -> false 
		|Node(a,b,c) -> if a=n then true 
						else if (mem n b) then true
						else (mem n c);; 



(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)


type nat = ZERO | SUCC of nat;;

let two = SUCC(SUCC(ZERO));;
let three = SUCC(SUCC(SUCC(ZERO)));;

let check_ZERO n = if n=ZERO then true else false;;

let rec natadd n1 n2 = 
	if check_ZERO(n1) then n2
	else
	begin
	match n2 with 
		|SUCC(n2) -> SUCC(natadd n1 n2)
		|ZERO -> 
			begin
		 		let rec cal dummy = 
		 			if check_ZERO(dummy) then ZERO
		 			else
		 			match dummy with
		 			|SUCC(n2) -> SUCC (cal n2)
          			(*if check_ZERO(dummy) then ZERO
          			else (*if SUCC(b) then*) SUCC(cal b)*)
          			|ZERO -> ZERO
          			in cal n1
         		end
	end;;

 let rec natmul n1 n2 =
 	if check_ZERO(n1) then ZERO
	else if check_ZERO(n2) then ZERO
	else
	begin
 		match n2 with
 		|SUCC(n2) -> natadd n1 (natmul n1 n2)
 		|ZERO -> ZERO
 	end;;
  	(*if check_ZERO(a) then ZERO 
  	else if check_ZERO(b) then ZERO 
  	SUCC(natadd a natmul(a b));;
	*)

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
= fun f -> true (* TODO *)

let rec getInt f = 
	match f with 
		|Num(n) -> n 
		|Plus(n1, n2) -> getInt(n1) + getInt(n2) 
		|Minus(n1, n2)-> getInt(n1) - getInt(n2);;


let rec eval f = 
	match f with
	|True   -> true
	|False  -> false
	|Not(a) -> if eval(a) then false else true
	|AndAlso (a,b) -> if eval(a) then begin if eval(b) then true else false end else false
	|OrElse  (a,b) -> if eval(a) then true else begin if eval(b) then true else false end
	|Imply   (a,b) -> if eval(a) then begin if eval(b) then true else false end else true
	|Equal   (a,b) -> if getInt(a)=getInt(b) then true else false;;
