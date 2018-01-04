type nat = ZERO
| SUCC of nat

let rec natadd (a, b) =
    match a with
    | ZERO -> b
    | SUCC n -> natadd(n, (SUCC b));;

let natmul (a, b) =
    let rec aux (a, b, result) =
        match a with
        | ZERO -> result
        | SUCC n -> aux(n, b, natadd(result, b))
    in aux(a, b, ZERO);;

(*
let rec nat_to_string n =
    match n with
    | ZERO -> "ZERO"
    | SUCC m -> ("SUCC ( " ^ nat_to_string(m) ^ " )");;

print_endline (nat_to_string (natmul(SUCC(SUCC (ZERO)), SUCC(SUCC(SUCC (ZERO))))));;
*)
