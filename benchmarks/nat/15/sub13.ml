
type nat = ZERO
        | SUCC of nat



let rec natadd : nat*nat -> nat = fun(n1, n2) -> 
    match n2 with
    | ZERO -> n1
    | SUCC(k) -> natadd(SUCC(n1), k)

let rec natmul : nat*nat -> nat = fun(n1, n2) ->
    match n2 with
    | ZERO -> ZERO
    | SUCC(k) -> natadd(n1, natmul(n1, k))


(*
let rec nat_to_int : nat -> int = fun n -> 
    match n with
    | ZERO -> 0
    | SUCC(n) -> 1 + nat_to_int(n)

let print_nat = fun n -> print_endline(string_of_int(nat_to_int(n)))


let _ = print_nat(natadd(SUCC(ZERO), SUCC(SUCC(ZERO))))

let a = SUCC(SUCC(SUCC(SUCC(ZERO))))
let b = SUCC(SUCC(ZERO))
let c = ZERO
let _ = print_nat(natadd(a, b))
let _ = print_nat(natmul(a, b))
let _ = print_nat(natmul(b, c))
let _ = print_nat(natmul(c, b))
*)
