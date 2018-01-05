(*print_endline "p4";;*)

type nat = ZERO | SUCC of nat

let rec natadd ((a: nat), (b: nat)): nat =
    match (a, b) with
    | (ZERO, _) -> b
    | (_, ZERO) -> a
    | (SUCC a', _) -> natadd (a', SUCC b)

let rec natmul' ((a: nat), (b: nat), (accum: nat)): nat =
    match (a, b) with
    | (ZERO, _) -> accum
    | (_, ZERO) -> accum
    | (SUCC a', _) -> natmul' (a', b, natadd (accum, b))

let natmul ((a: nat), (b: nat)): nat = natmul' (a, b, ZERO)


(*;;*)

(*let rec iter ((n: int), (f: 'a->'a)): ('a->'a) =*)
    (*match n with*)
    (*| n when n < 0 -> fun x -> x*)
    (*| 0 -> fun x -> x*)
    (*| n -> fun x -> x |> f |> iter ((n-1), f)*)

(*let rec int_of_nat' (n: nat) (accum: int): int =*)
    (*match n with*)
    (*| ZERO -> accum*)
    (*| SUCC n' -> int_of_nat' n' (accum+1)*)

(*let string_of_nat (n: nat): string =*)
    (*int_of_nat' n 0 |> string_of_int*)

(*;; print_endline ""*)
(*; (ZERO, ZERO) |> natadd |> string_of_nat |> print_endline*)

(*let succ (n: nat): nat = SUCC n*)

(*let nat_of_int (n: int) = ZERO |> iter (n, succ)*)

(*;; print_endline ""*)
(*; ((nat_of_int 10), (nat_of_int 7)) |> natadd |> string_of_nat |> print_endline*)
(*;; print_endline ""*)
(*; ((nat_of_int 10), (nat_of_int 7)) |> natmul |> string_of_nat |> print_endline*)

