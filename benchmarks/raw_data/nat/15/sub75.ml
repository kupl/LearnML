type nat = ZERO | SUCC of nat

let rec i2nat : int -> nat
=fun i -> match i with
    | 0 -> ZERO
    | n -> SUCC (i2nat (n-1))

let rec nat2i : nat -> int
=fun n -> match n with
    | ZERO -> 0
    | SUCC rem -> 1 + nat2i rem

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n2 with
    | ZERO -> n1
    | SUCC n -> SUCC (natadd n1 n);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> i2nat ((nat2i n1) * (nat2i n2));;

(*
print_string("Pascal Test\n");;
if pascal (0,0) != 1 then print_string("fail");;
if pascal (1,0) != 1 then print_string("fail");;
if pascal (1,1) != 1 then print_string("fail");;
if pascal (2,1) != 2 then print_string("fail");;
if pascal (4,2) != 6 then print_string("fail");;
print_string("----\n\n");;

print_string("Sigma Test\n");;
if sigma (fun x -> x) 1 10 != 55 then print_string("fail");;
if sigma (fun x -> x*x) 1 7 != 140 then print_string("fail");;
print_string("----\n\n");;

print_string("Max, Min Test\n");;
if max [1;3;5;2] != 5 then print_string("fail");;
if min [1;3;2] != 1 then print_string("fail");;
print_string("----\n\n");;

print_string("Prop Test\n");;
if eval False != false then print_string("fail");;
if eval True != true then print_string("fail");;
if eval (Neg False) != true then print_string("fail");;
if eval (Or (True, True)) != true then print_string("fail");;
if eval (Or (True, False)) != true then print_string("fail");;
if eval (Or (False, False)) != false then print_string("fail");;
if eval (And (True, True)) != true then print_string("fail");;
if eval (And (True, False)) != false then print_string("fail");;
if eval (And (False, False)) != false then print_string("fail");;
if eval (Imply (True, True)) != true then print_string("fail");;
if eval (Imply (True, False)) != false then print_string("fail");;
if eval (Imply (False, False)) != true then print_string("fail");;
if eval (Equiv (True, True)) != true then print_string("fail");;
if eval (Equiv (True, False)) != false then print_string("fail");;
if eval (Equiv (False, False)) != true then print_string("fail");;
if eval (Or (False, And (True, Imply ((Neg (Equiv (False, False))), False)))) != true then print_string("fail!!");;
print_string("----\n\n");;
*)
