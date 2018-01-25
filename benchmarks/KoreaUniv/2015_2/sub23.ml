(*********************) (* Problem 1: filter *) (*********************) let rec filter pred lst = [] let rec filter f a = 
	match a with
	[] -> raise(Failure"List is empty")
	| hd::tl -> if ( f hd ) then hd::(filter f tl) else
	filter f tl

(*********************) (* Problem 2: zipper *) (*********************) let rec zipper : int list * int list -> int list =fun (a,b) -> [] let rec zipper a b = 
	match a with
	[] -> b
	| hd::tl -> match b with
	[] -> a
	| h::t -> hd::h::(zipper tl t)

(*******************) (* Problem 3: iter *) (*******************) let rec iter : int * (int -> int) -> (int -> int) =fun (n,f) -> f let rec iter (n,f) =
	fun a ->
	if n = 0 then a else
	iter (n-1,f) (f a)

(*********************)(* Problem 4: Diff *) (*********************) type aexp = | Const of int | Var of string | Power of string * int | Times of aexp list | Sum of aexp list let rec diff : aexp * string -> aexp =fun (aexp,x) -> aexp let rec diff a b =
	match a with
	Const a -> Const 0
	| Var l -> if l = b then Const 1
	else Const 0
	| Power (a1,b1) -> if b = a1 then
		if b1 = 2 then
		Times [Const 2; Var a1]
		else
		Times [Const b1; Power(a1,b1-1)]
	else
		Const 0
	| Times [hd;tl] -> Times[hd;(diff tl b)]
	| Sum p -> match p with
	[x] -> (diff x b)
	| hd :: tl -> Sum [(diff hd b);(diff (Sum tl) b) ]
	
(*************************) (* Problem 5: Calculator *) (*************************) type exp = X | INT of int | ADD of exp * exp | SUB of exp * exp | MUL of exp * exp | DIV of exp * exp | SIGMA of exp * exp * exp let calculator : exp -> int =fun e -> 0
let rec cal p x =
	match p with
	INT a -> a
	| X -> x
	| ADD(a,b) -> cal a x + cal b x
	| SUB(a,b) -> cal a x - cal b x
	| MUL(a,b) -> cal a x * cal b x
	| DIV(a,b) -> cal a x / cal b x
	| SIGMA(a,b,c) -> if a = b then (cal c (cal a 0))
	else cal (ADD(INT (cal c (cal a 0)), SIGMA(INT ((cal a 0) + 1),b,c))) 0

let calculator p =
	cal p 0