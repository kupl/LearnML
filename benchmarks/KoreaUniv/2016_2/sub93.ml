(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: hd' :: tl ->
if hd > hd' then max(hd :: tl)
else max(hd' :: tl);;


let rec min : int list -> int
= fun lst -> match lst with
| [hd] -> hd
| hd :: hd' ::tl ->
if hd < hd' then min(hd :: tl)
else min(hd' :: tl);;


(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = match lst with
|[] -> []
|hd::tl ->
let new_tl = filter pred tl in
if pred hd then hd::new_tl else new_tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
f(f a);;
(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node(nt, l, r) ->
if nt = n then true
else (mem n l) && (mem n r);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> n1
| SUCC(tl) -> 
if tl = ZERO then SUCC n1
else natadd (SUCC(n1)) tl;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n2 with
| ZERO -> ZERO
| SUCC(ZERO) -> n1
| SUCC(tl) -> natadd (natmul n1 tl) n1;;

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
= fun f -> 
let rec exptoint : exp -> int
=fun f1->
match f1 with 
| Num fs -> fs
| Plus (fs, ls) -> exptoint fs + exptoint ls
| Minus (fs, ls) -> exptoint fs - exptoint ls in
match f with
| True -> true
| False -> false
| AndAlso(fs, ls) ->
if fs = False then false
else if ls = False then false
else true
| OrElse(fst, lst)->
if fst = True then true
else if lst = True then true
else false
| Imply(fst, lst) ->
if fst = True && lst = False then false
else true
| Equal(fs, ls) -> if exptoint fs = exptoint ls then true else false
| Not(a1) -> not (eval a1);;