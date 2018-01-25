(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
  List.fold_left
    (fun a b ->
      if a > b then
        a
      else
        b)
  (List.hd lst)
  lst;;


let rec min : int list -> int
= fun lst ->
  List.fold_left (fun a b ->
    if a < b then
      a
    else
      b)
  (List.hd lst)
  lst;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
   match lst with
   |[] -> []
   |hd :: tl ->
    if pred hd then
      hd :: (filter pred tl)
    else
      filter pred tl;;

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
   |Node(a, b, c) ->
      if a = n then  true else (mem n b) || (mem n c);;

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
   |SUCC a-> SUCC(natadd a n2);;

let  rec natmul : nat -> nat -> nat
= fun n1 n2 ->
   match n2 with
   |ZERO -> ZERO
   |SUCC a -> natadd n1 (natmul n1 a);;

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
   | Not form -> if eval form = true then
        false
      else
        true
   | AndAlso (form1, form2) ->
      if eval form1 = true && eval form2 = true then
        true
      else
        false
   | OrElse (form1, form2) ->
      if eval form1 = false && eval form2 = false then
        false
      else
        true
   | Imply (form1, form2) ->
      if eval form1 = true && eval form2 = false then
        false
      else
        true
   | Equal (ex1, ex2) ->
   	let rec eti e =
   	match e with
       |Num n -> n
       |Plus (n, l) -> (eti n) + (eti l)
       |Minus (n, l) -> (eti n) - (eti l) in
        if (eti ex1) = (eti ex2) then
          true
        else
          false;;
