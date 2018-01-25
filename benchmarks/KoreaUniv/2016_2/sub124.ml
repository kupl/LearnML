(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 0 (* TODO *)

let rec fold f l a=
	match l with
	| [] -> a
	| hd::tl -> f hd (fold f tl a);;

let sum l = fold (+) l 0;;
let prod l = fold (fun x y -> x * y)l 1;;

let max lst = fold(fun x y -> if(x>=y) then x else y) lst 0;;

let rec min : int list -> int
= fun lst -> 0 (* TODO *)

let min lst = fold(fun x y -> if(x<=y) then x else y) lst (max lst);;


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = [];; (* TODO *)

let rec  filter pred lst =
	match lst with
  |	[] -> []
	| hd::tl ->if(pred hd)then hd:: filter pred tl else filter pred tl ;;


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a (* TODO *)



let rec double f a =
	f (f a);;




(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true (* TODO *)

let rec mem n tree =
	match tree with
	| Empty -> false
	| Node(c, left, right) ->
		if n = c then true
		else mem n left || mem n right;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

let rec natadd n1 n2 =
	match n1 with
	| ZERO -> n2
	| SUCC n3 -> SUCC (natadd n3 n2);;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)


let rec natmul n1 n2 = 
	match n2 with
	| ZERO -> ZERO
	| SUCC n3 -> natadd n1 (natmul n1 n3);; 

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

let rec expl : exp -> int
= fun e -> 0

let rec expl e =
	match e with
	| Num n -> n
	| Plus (e1,e2) -> expl e1 + expl e2
	| Minus (e1,e2) -> expl e1 - expl e2;;

let rec eval f=
	match f with
	| True -> True
	| False ->False
	| Not f1 -> if(f1= True) then False else True
	| AndAlso (f1, f2) -> if(f1=True && f2 =True) then True else False
	| OrElse (f1, f2) -> if(f1=False && f2= False)then False else True
	| Imply (f1, f2) -> if(f2 = False && f1 = True) then False else True
	| Equal  (e1, e2) -> if(expl e1==expl e2) then True else False;;		
