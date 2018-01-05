type nat = ZERO
| SUCC of nat

let rec natadd (a, b) =
    match a with
    | ZERO -> b
    | SUCC n -> natadd(n, (SUCC b));;

let rec natmul (a, b) =
    match a with
    | ZERO -> ZERO
    | SUCC n -> natadd( b, natmul(n, b));;

(*
let rec nat_to_string n =
    match n with
    | ZERO -> "ZERO"
    | SUCC m -> ("SUCC ( " ^ nat_to_string(m) ^ " )");;

print_endline (nat_to_string (natmul(SUCC(SUCC (ZERO)), SUCC(SUCC(SUCC (ZERO))))));;
*)
