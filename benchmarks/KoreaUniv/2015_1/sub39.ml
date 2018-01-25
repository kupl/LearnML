(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
		if y=0 then 1
		else if x=y then 1
		else pascal(x-1,y-1) + pascal (x-1,y)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
		if b=0 then 0
		else if a>b then 0
		else if a=b then f a
		else ((f a) + sigma f (a+1) b)

(* Problem 3 *)
let rec max : int list -> int
=fun l ->
		match l with
		|[]->0
		|hd::[]->hd
		|hd::tl->
			if hd>max tl then hd
			else  max tl

let rec min : int list -> int
=fun l ->
		match l with
		|[]->0
		|hd::[] -> hd
		|hd::tl ->
			if hd<min tl then hd
			else  min tl

(* Problem 4 *)
type formula =
		True
	| False
	| Neg of formula
	| Or of formula * formula
	| And of formula * formula
	| Imply of formula * formula
	| Equiv of formula * formula

let rec eval : formula -> bool
=fun f->
		match f with
		|True -> true
		|False -> false
		|Neg form1 -> not (eval (form1))
		|Or (form1,form2) -> eval(form1) ||eval( form2)
		|And (form1,form2) ->eval( form1) && eval(form2)
		|Imply (form1,form2) ->
			if (eval(form1)=false) then true
			else if (eval(form2)) then true
			else false
		|Equiv (form1, form2)->
			if (eval(form1)=eval(form2)) then true
			else false

(* Problem 5 *)

type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun su1 su2->
		match su1, su2 with
		| ZERO, ZERO -> ZERO
		| SUCC(num1), ZERO -> su1
		| ZERO, SUCC(num2)-> su2
		| SUCC(num1), SUCC(num2) -> SUCC(SUCC(natadd num1 num2)

let rec natmul : nat -> nat -> nat
=fun su3 su4->
		match su3, su4  with
		|ZERO, ZERO -> ZERO
		|SUCC(num3), ZERO -> ZERO
		|ZERO, SUCC(num4) -> ZERO
		|SUCC(num3), SUCC(num4) -> natadd(natmul num3 su4) su4


