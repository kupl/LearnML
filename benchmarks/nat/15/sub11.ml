
type nat = ZERO
        | SUCC of nat

let rec nat_to_int : nat -> int = fun n -> 
    match n with
    | ZERO -> 0
    | SUCC(n) -> 1 + nat_to_int(n)

let rec int_to_nat : int -> nat = fun i ->
    if i == 0 then ZERO
    else SUCC(int_to_nat(i-1))

let print_nat = fun n -> print_endline(string_of_int(nat_to_int(n)))


let natadd : nat*nat -> nat = fun(n1, n2) -> int_to_nat(nat_to_int(n1) + nat_to_int(n2))
let natmul : nat*nat -> nat = fun(n1, n2) -> int_to_nat(nat_to_int(n1) * nat_to_int(n2))

(*

let _ = print_nat(natadd(SUCC(ZERO), SUCC(SUCC(ZERO))))

let a = SUCC(SUCC(SUCC(SUCC(ZERO))))
let b = SUCC(ZERO)
let _ = print_nat(natadd(a, b))
let _ = print_nat(natmul(a, b))

*)
