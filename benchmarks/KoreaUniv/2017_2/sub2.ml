(*(*Problem 1*)
type btree = Empty | Node of int * btree * btree;;

let rec mirror bt = function l ->
 let empty = Empty in
  match l with
  [] -> empty
*)

(*Problem 2*)
type nat = ZERO | SUCC of nat
let rec natadd n1 n2 =
 match n1 with
  ZERO -> n2
  | SUCC x -> SUCC( natadd x n2)
let rec natmul n1 n2 = 
 match n1 with
 ZERO -> ZERO
 |SUCC x -> natadd n2 (natmul x n2)
let rec natexp n1 n2 = 
 match n2 with
 ZERO -> SUCC ZERO
 |SUCC x -> natmul n1 (natmul x n1)

(*Problem 3*)

