(* problem 1*)
 type btree = Empty|Node of int * btree * btree
  let mirror : btree -> btree
  = fun t ->
 match t with
 Empty -> Empty
 | Node(a,b,c) -> Node(a,c,b)

 (* problem 2*)
 type nat = ZERO | SUCC of nat
 let one = SUCC ZERO
 let rec trans : nat -> int
 = fun x ->
 match x with
  ZERO -> 0
 | SUCC(x) -> 1+ trans x

 let rec ret : int -> nat
 = fun y ->
 match y with
 0 -> ZERO
 |_ -> SUCC (ret (y-1))

 let rec natadd : nat -> nat -> nat
 = fun n1 n2 ->
 let a = trans n1 in
 let b = trans n2 in
 ret (a+b)

  let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
 let a = trans n1 in
 let b = trans n2 in
 ret (a*b)

 let natexp : nat ->  nat -> nat
  = fun n1 n2 ->
 let a = trans n1 in
 let b = trans n2 in
 let rec exp a b =
 match b with
 0 -> 1
 |_ -> a*(exp a (b-1)) in
 ret (exp a b)

 (* problem 3*)
 type formula = True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

 let rec sat : formula -> bool
 = fun f ->
match f with
 True -> true
 |False -> false
 |Var a ->( match a with "P" -> true |_ -> false )
 |Neg s -> if s=True then false else true
 |Or (a, b) -> if (a=False)&&(b=False) then false else true
 |And (a, b) -> if (a=True)&&(b=True) then true else false
 |Imply (a, b) -> if a=True&&b=False then false else true
 |Iff (a, b) -> if a==b then true else false
(*
 (* problem 4*)
 type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

 let rec diff : aexp * string -> aexp
 = fun (e,x) ->
 match (e,x) with
 | Power (e, x) ->( match x with
   |0 -> 1
   |_ -> e*(Power (e,x-1)))
 | Times lst -> (match lst with
   | []->1
   | hd::tl -> hd*(Times tl))
 | Sum lst ->
  ( match lst with
  |[] -> 0
  | h::t -> h+(Sum t))*)
