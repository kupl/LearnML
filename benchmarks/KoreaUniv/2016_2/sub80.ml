(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = match l with
										|[]->a
										|hd::tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> fold (fun a b -> if a>b then a else b) lst min_int;;


let rec min : int list -> int
= fun lst -> fold (fun a b -> if a>b then b else a ) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
													|[]->[]
													|hd::tl->if pred hd then hd::(filter pred tl)
																							else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  match tree with |Empty ->false | Node(b,c,d) ->(n=b)||(mem n c )||(mem n d);;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  match n2 with |ZERO ->n1 |SUCC c -> SUCC(natadd n1 c);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	|ZERO -> ZERO
	|SUCC(c) -> natadd n1 (natmul n1 c);;

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

let rec eee : exp -> int
= fun e -> match e with | Num b -> b | Plus(b,c) ->(eee b)+(eee c)
												| Minus(b,c) -> (eee b)-(eee c);;

let rec eval : formula -> bool
= fun f -> match f with |True ->true |False ->false|Not a ->not (eval a)
												|AndAlso (a,b) -> (eval a)&& (eval b)
												|OrElse (a,b) -> (eval a)|| (eval b)
												|Imply(a,b) -> if ((eval a) = true)&&((eval b) = false) 																			 then false else true
												|Equal(a,b) -> (eee a) = (eee b) ;;
