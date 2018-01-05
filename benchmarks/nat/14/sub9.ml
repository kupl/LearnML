type nat = ZERO | SUCC of nat

let rec natadd nats =
    match nats with
    | (ZERO, nat1) -> nat1
    | (SUCC nat1, nat2) -> natadd (nat1, SUCC nat2)
    ;;

let rec natmul nats =
    match nats with
    | (ZERO, ZERO) -> ZERO
    | (nat1, ZERO) -> ZERO
    | (ZERO, nat1) -> ZERO
    | (SUCC ZERO, nat1) -> nat1
    | (SUCC nat1, nat2) -> natadd (nat2, natmul(nat1, nat2))
    ;;

(*
let rec toInt nat =
    match nat with
    | (ZERO) -> 0
    | (SUCC lRat) -> 1 + toInt lRat
    ;;

let zero =ZERO;;
let one = (SUCC ZERO);;
let two = (SUCC (SUCC ZERO));;
let three = (SUCC (SUCC (SUCC ZERO)));;
let four = (SUCC (SUCC (SUCC (SUCC ZERO))));;
let five = (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))));;

print_endline (string_of_int (toInt (natmul (zero, zero))));

print_endline (string_of_int (toInt (natmul (one, zero))));
print_endline (string_of_int (toInt (natmul (one, one))));
print_endline (string_of_int (toInt (natmul (one, two))));

print_endline (string_of_int (toInt (natmul (three, zero))));

print_endline (string_of_int (toInt (natmul (three, one))));
print_endline (string_of_int (toInt (natmul (three, five))));
print_endline (string_of_int (toInt (natmul (four, five))));

let _ = 
let rec nat_to_int : nat -> int = 
fun n -> 
match n with 
| ZERO -> 0 
| SUCC n1 -> 1 + nat_to_int n1 
in 

let print_bool x = 
print_endline (string_of_bool x) 
in 

let three = SUCC (SUCC (SUCC ZERO)) 
in 
let four = SUCC three 
in 

print_bool (7 = nat_to_int (natadd (three, four))); 
print_bool (0 = nat_to_int (natadd (ZERO, ZERO))); 
print_bool (3 = nat_to_int (natadd (ZERO, three))); 
print_bool (4 = nat_to_int (natadd (four, ZERO))); 

print_bool (12 = nat_to_int (natmul (three, four))); 
print_bool (0 = nat_to_int (natmul (ZERO, three))); 
print_bool (0 = nat_to_int (natmul (four, ZERO))); 
print_bool (0 = nat_to_int (natmul (ZERO, ZERO))); 
print_bool (3 = nat_to_int (natmul (SUCC ZERO, three))); 
print_bool (4 = nat_to_int (natmul (four, SUCC ZERO)));
*)
