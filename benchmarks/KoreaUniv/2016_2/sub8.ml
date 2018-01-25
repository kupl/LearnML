(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
match l with 
|[] -> a
|hd::tl -> fold f tl (f hd a)

let rec max l =
let a = -1073741824 in
fold (fun x y -> if x>y then x else y) l a

let rec min l =
let a = 1073741824 in
fold (fun x y -> if x<y then x else y) l a


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter p l =
match l with
|[] -> []
|hd::tl -> if (p hd) == true then hd::filter p tl else filter p tl

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
f (f a)

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n bt =
match bt with
|Empty -> false
|Node (x, b1, b2) -> if x == n then true else (mem n b1 || mem n b2)


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd a b =
match b with
|ZERO -> a
|SUCC n -> natadd (SUCC a) n 

let natdec a = 
match a with
| ZERO -> raise (Failure "Cannot go below ZERO")
| SUCC nat -> nat

let rec nataddntimes a n b =
match n with
|ZERO -> ZERO
|SUCC ZERO -> a
|SUCC nat -> nataddntimes (natadd a b) (natdec n) b

let rec natmul a b =
nataddntimes a b a

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

let rec evalex e =
match e with 
| Num n -> n
| Plus (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 n1 + n2
 end
| Minus (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 n1 - n2
 end

let rec eval f =
match f with
| True -> true
| False -> false
| Not f1 ->
 begin
 let b1 = eval f1 in
 if b1 == true then false else true
 end
| AndAlso (f1, f2) ->
 begin
 let b1 = eval f1 in
 let b2 = eval f2 in
 b1 && b2
 end
| OrElse (f1, f2) ->
 begin
 let b1 = eval f1 in
 let b2 = eval f2 in
 b1 || b2
 end
| Imply (f1, f2) -> 
 begin 
 let b1 = eval f1 in
 let b2 = eval f2 in
 if b1 == true && b2 == false then false else true
 end
| Equal (e1, e2) ->
 begin
 let n1 = evalex e1 in
 let n2 = evalex e2 in
 if n1 == n2 then true else false
 end

