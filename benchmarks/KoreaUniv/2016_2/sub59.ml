(*********************)
(*     Problem 1     *)
(*********************)
let rec max lst=
match lst with 
[]->0
| [a]-> a
| hd::tl -> if hd > (max tl) then hd
            else (max tl);;  

let rec min lst=
match lst with 
[]->0
| [a]->a
| hd::tl -> if hd < (min tl) then hd
            else (min tl);;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
match lst with 
[] -> []
|hd::tl -> if (pred hd) = true then hd::(filter pred tl)
           else filter pred tl;; 

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
	| Empty
	| Node of int * btree * btree;;

(*
let rec mem n tree=
match tree with
 
Node(n, Empty, Empty)-> true;;*)
(*
| Node (a, Empty, Empty)-> false
| Node (a, e1, e2)-> 
     let e1= btree in 
     let e2= btree in 
     match ->(mem n e1)|| (mem n e2);; *)

(*********************)
(*     Problem 5     *)
(*********************)
(*
type nat =
	| ZERO
	| SUCC of nat

let rec natadd a b =
match a b with 
if a= ZERO then b
else a= SUCC(_,;;



let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO (* TODO *)

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

let rec eval f 
if f= true then true 
else false*)
