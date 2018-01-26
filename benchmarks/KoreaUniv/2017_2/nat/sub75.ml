(* problem 2*)
type nat = ZERO | SUCC of nat

let rec num n = 
match n with
| ZERO -> 0
| SUCC ZERO -> 1
| SUCC a -> 1+(num a);;

let rec make n =
match n with
| 0 -> ZERO
| 1 -> SUCC ZERO
| _ -> SUCC(make (n-1));;

let rec exp n1 n2 = 
match n1 with
| 0 -> 0
| 1 -> 1
| _ ->
if n2 = 0 then 1 else if n2 = 1 then n1 else n1*(exp n1 (n2-1));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> make ((num n1)+(num n2));;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> make ((num n1)*(num n2));;

let natexp : nat -> nat -> nat 
= fun n1 n2 -> make (exp (num n1) (num n2));;
