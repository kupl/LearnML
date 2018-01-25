let rec map f l =
	match l with
	|[] ->[]
	|hd::tl -> (if (f hd)=true then [hd] else [])@(map f tl)

let rec fold f l a =
	match l with
	|[] -> a
	|hd::tl -> f hd (fold f tl a)

(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> 
	match lst with
	|[] -> raise (Failure "Input is not correct")
	|hd::tl -> fold (fun x y -> if x>y then x else y) lst hd

let rec min : int list -> int
= fun lst ->
	match lst with
	|[] -> raise (Failure "Input is not correct")
	|hd::tl -> fold (fun x y -> if x<y then x else y) lst hd

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = map pred lst

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	|Empty -> false
	|Node (k, bt1, bt2) ->
		if n=k then true
		else if (mem n bt1)=true then true
		else mem n bt2

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec iton n =
	match n with
	| ZERO -> 0
	| SUCC tl -> 1+iton tl

let rec comp i n = if (iton n)=i then n else comp i (SUCC (n))

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let v1 = iton n1 in
	let v2 = iton n2 in
	comp (v1+v2) ZERO

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let v1 = iton n1 in
	let v2 = iton n2 in
	comp (v1*v2) ZERO

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

let rec eval2: exp -> int
= fun e ->
	match e with
	|Num n -> n
	|Plus (n1,n2) -> (eval2 n1)+(eval2 n2)
	|Minus (n1,n2) -> (eval2 n1)-(eval2 n2)

let rec eval : formula -> bool
= fun f -> 
	match f with
	|True ->true
	|False ->false
	|Not f -> not(eval f)
	|AndAlso (f1,f2) -> (eval f1)&&(eval f2)
	|OrElse (f1,f2) -> (eval f1)||(eval f2)
	|Imply (f1,f2) -> 
		begin
			match (eval f1), (eval f2) with
			|true,true->true
			|true,false->false
			|false,_->true
		end
	|Equal (e1,e2) -> (eval2 e1)=(eval2 e2)

