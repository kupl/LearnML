(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a = 
	match l with 
	|[]->a
	|hd::tl->f hd (fold f tl a)

let rec max : int list -> int
= fun lst -> 0 (* TODO *)
let max lst = fold (fun x y -> if x>y then x else y) lst 0

let rec min : int list -> int
= fun lst -> 0 (* TODO *)
let min lst = fold (fun x y -> if x<y then x else y) lst (max lst)


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = [] (* TODO *)
let filter (fn : 'a->bool)(xs: 'a list): 'a list =
	let rec aux xs acc =
		match xs with 
		| []->acc
		| y::ys-> if(fn y) then (aux ys (acc@[y])) then (aux ys acc) in
	aux xs []	


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = a (* TODO *)
let rec double f a= f (f x)


(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> true (* TODO *)
let rec mem x btree =
	match btree with
	|Empty -> False
	|Node (y, left, right) -> if x=y then true else if x<y then mem x left else mem x right


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO 
let rec natadd n1 n2 = 
	match n2 with
	|ZERO -> 
		let rec e n = 
			match n with
			|ZERO -> ZERO
			|SUCC (nk) -> SUCC (e nk)
		in
		e n1
	|SUCC (k) -> SUCC (natadd n1 k)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO 
let rec natmul n1 n2 =
	match n2 with
	|ZERO -> ZERO
	|SUCC (k) -> natadd n1 (natmul n1 k)


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
let rec eval f =
	match f with
	|True -> true
	|False -> false
	|Not a -> not (eval a)
	|AndAlso(a,b) -> eval (a) && eval (b)
	|OrElse(a,b) -> eval (a) || eval (b)
	|Imply(a,b) -> if eval (a) then eval (b) else true
	|Equal(a,b) ->
		let rec eval2 e =
			match e with
			|Num n -> n
			|Plus (e1, e2) -> eval2 (e1) + eval2 (e2)
			|Minus (e1, e2) -> eval2 (e1) - eval2 (e2)
			in
		if eval2(a) = eval2(b) then true else false