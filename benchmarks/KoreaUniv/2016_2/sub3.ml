(*********************)
(*     Problem 1     *)
(*********************)
let rec fold f l =
  match l with
    | [] -> raise(Failure "list size is 0")
    | h::[] -> h
    | h::t -> f h (fold f t);;

let cmp_b a b =
  if a>=b then a
  else b;;

let cmp_s a b =
  if a>=b then b
  else a;;

let rec max l = fold (cmp_b) l;;

let rec min l = fold (cmp_s) l;;

(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst = 
  match lst with
    |  [] -> []
    |  h::t ->
if pred h == true then h::(filter pred t)
else filter pred t;;

(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a = 
f (f a);;

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
  | Empty
  | Node of int * btree * btree

let rec mem n tree =
  match tree with
    | Empty -> false
    | Node(a,b,c) ->
if n==a then true
else if (mem n b)==true then true
else mem n c;;


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
  | ZERO
  | SUCC of nat

let rec natadd n1 n2 =
  match n2 with
    | ZERO -> n1
    | (SUCC n3) -> natadd (SUCC n1) n3;;

let rec natmul n1 n2 =
  match n2 with
    | ZERO -> ZERO
    | (SUCC ZERO) -> n1
    | (SUCC n3) -> natadd n1 (natmul n1 n3);;

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

let rec cal e = 
  match e with
    | Num(e1) -> e1
    | Plus(e1,e2) -> (cal e1)+(cal e2)
    | Minus(e1,e2) -> (cal e1)-(cal e2);;

let rec eval f =
  match f with
    | True -> true
    | False -> false
    | Not(f1) -> 
      if (eval f1)==true then false
      else true
    | AndAlso(f1,f2) ->
      if (eval f1)==true && (eval f2)==true then true 
      else false
    | OrElse(f1,f2) -> 
      if (eval f1)==false && (eval f2)==false then false 
      else true
    | Imply(f1,f2) -> 
      if (eval f1)==true && (eval f2)==false then false 
      else true 
    | Equal(e1,e2) -> 
      if (cal e1)==(cal e2) then true 
      else false;;