(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
   match l with
   |[] -> a
   |hd :: tl -> f hd (fold f tl a);;

let rec max : int list -> int
= fun lst -> fold (fun x y -> if x > y then x else y) lst min_int;; 


let rec min : int list -> int
= fun lst -> fold (fun x y -> if x < y then x else y) lst max_int;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
   match lst with
   |[] -> []
   |hd :: tl ->
   if pred hd then hd :: (filter pred tl) else filter pred tl;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = f (f a);;

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
   |Node(k, l, m) -> 
      if k = n then  true else (mem n l) || (mem n m);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat =
   | ZERO
   | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
   match n1 with
   |ZERO -> n2
   |SUCC k-> SUCC(natadd k n2);;

let  rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
   match n2 with
   |ZERO -> ZERO
   |SUCC k -> natadd n1 (natmul n1 k);;

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
   match f with
   | True -> true
   | False -> false
   | Not fo -> if eval fo = true then false else true
   | AndAlso (fo1, fo2) -> if eval fo1 = true && eval fo2 = true then true else false
   | OrElse (fo1, fo2) -> if eval fo1 = false && eval fo2 = false then false else true
   | Imply (fo1, fo2) -> if eval fo1 = true && eval fo2 = false then false else true
   | Equal (ex1, ex2) -> 
   	let rec eti e = 
   	match e with 
       |Num k -> k
       |Plus (k, l) -> (eti k) + (eti l)
       |Minus (k, l) -> (eti k) - (eti l) in
      if (eti ex1) = (eti ex2) then true else false;;
