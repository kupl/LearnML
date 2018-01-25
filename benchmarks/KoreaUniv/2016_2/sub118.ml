(*2014130211 Donghyun Koh*)



(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l a =
  match l with
    | [] -> a
    | hd::tl -> f hd (fold f tl a)

let rec max : int list -> int
  = fun lst -> 
    let large a b =
      if a > b then a
      else b in fold (large)lst 0

let rec min : int list -> int
  = fun lst -> 
    let small a b =
      if a > b then b
      else a in fold (small)lst 99999



(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =  (* (’a -> bool) -> ’a list -> ’a list  *)
  match lst with
    |[] -> []
    |h::t -> 
        match (pred h) with
          |true ->h::(filter pred t)
          |false -> (filter pred t)



(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a  (* (’a -> ’a) -> ’a -> ’a *)
  = f (f a)



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
      |Node (int, Empty , Empty) -> n = int
      |Node (int, Empty, b1) -> (mem n b1)|| n = int
      |Node (int, b1, Empty) -> (mem n b1) || n = int
      |Node (int, b1, b2) -> (mem n b1) || (mem n b2) ||n = int



(*********************)
(*     Problem 5     *)
(*********************)
type nat =
    	| ZERO
    	| SUCC of nat

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
    match (n1, n2) with
      |(ZERO,_) -> n2
      |(SUCC(s1),ZERO) -> (natadd s1 (SUCC ZERO))
      |(SUCC(s1),s2) -> (natadd s1 (SUCC(s2)))

let rec natmul : nat -> nat -> nat
  = fun n1 n2 -> 
    match (n1,n2) with
      |(ZERO,_) -> ZERO
      |(_,ZERO) -> ZERO
      |(s1,(SUCC ZERO)) -> s1
      |((SUCC ZERO), s2) -> s2
      |(s1, SUCC(s2)) -> (natadd (natmul s1 s2) n1)



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

