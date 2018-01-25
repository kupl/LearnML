(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
 match l with
 |[] -> a
 |hd::tl -> f hd(fold f tl a);;

let rec max : int list -> int
= fun lst -> 
 fold (fun x y -> if x > y then x else y) lst 0 ;;
 
let rec min : int list -> int
= fun lst -> 
 fold (fun x y -> if x < y then x else y) lst 0 ;;

(*let rec map f l =
 match l with 
 |[] -> []
 |hd::tl -> (f hd)::(map f tl);;*)

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
 match lst with
|[] -> []
|hd::tl -> if(pred hd) then hd::(filter pred tl)
		 else (filter pred tl);;


(*********************)
(*     Problem 3     *)
(*********************)
let inc x = x + 1;;
let mul x = x * 2;;
let rec double f a = 
 f(f a);; 

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree;;

(*let t1 = Node (1, Empty, Empty);;*)
(*let t2 = Node (1, Node(2,Empty,Empty), Node(3,Empty,Empty));;*)

let rec mem : int -> btree -> bool
= fun n tree -> 
  match tree with
  |Empty -> false 
  |Node(_,Empty,_) ->
    if n = 1 then true else false
  |Node(_,left,right) -> mem (n/2) left;;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat
(*let two = SUCC(SUCC ZERO);;*)
(*let three = SUCC(SUCC(SUCC ZERO));;*)

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
 match n1 with
  |ZERO -> n2
  |SUCC(nest) -> SUCC(natadd nest n2);; 

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
 match n1, n2 with 
 |_,ZERO -> SUCC(ZERO)
 |ZERO,SUCC(nest2) -> SUCC(natmul n1 nest2)
 |SUCC(nest1),_ -> SUCC(natmul nest1 n2);;

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

let rec match_exp : exp -> int
 = fun e ->
	match e with
	|Num e1 -> e1
	|Plus (e1, e2) -> match_exp e1 + match_exp e2
	|Minus (e1, e2) -> match_exp e1 - match_exp e2;;

let rec eval : formula -> bool
= fun f -> 
  match f with
  |True -> true
  |False -> false 
  |Not n -> not(eval n)
  |AndAlso (fo1,fo2) -> (eval fo1) && (eval fo2)
  |OrElse (fo1,fo2) -> (eval fo1) || (eval fo2)
  |Imply (fo1,fo2) -> not (eval fo1) || (eval fo2)  
  |Equal (e1,e2) -> 
	let v1 = match_exp e1 in
	let v2 = match_exp e2 in
	if v1 = v2 then true else false;;



