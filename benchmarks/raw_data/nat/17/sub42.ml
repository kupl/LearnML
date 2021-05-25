(* problem 2*)
 type nat = ZERO | SUCC of nat
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
