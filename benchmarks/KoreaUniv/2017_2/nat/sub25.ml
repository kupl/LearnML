(* problem 2*)
type nat = ZERO | SUCC of nat

let rec how_many : nat -> int
= fun n -> match n with
            | ZERO -> 0
            | SUCC(x) -> 1 + how_many(x)

let rec make_answer : int -> nat
= fun n -> if n = 0 then ZERO
           else SUCC(make_answer (n-1))

let natadd : nat -> nat -> nat
= fun n1 n2 -> make_answer ((how_many n1) + (how_many n2))

let natmul : nat -> nat -> nat
= fun n1 n2 -> make_answer ((how_many n1) * (how_many n2))

let natexp : nat -> nat -> nat
= fun n1 n2 -> make_answer (int_of_float (float_of_int(how_many n1) ** float_of_int(how_many n2)))
