(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
 in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
  in f2 ((f n1)+(f n2))

let natmul : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
              in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
  in f2 ((f n1)*(f n2))


let natexp : nat -> nat -> nat
= fun n1 n2 -> let rec f n = match n with
                            | ZERO -> 0
                            | SUCC(b) -> f b + 1
              in let rec f2 n = if n = 0 then ZERO else SUCC(f2 (n-1))
              in let rec f3 a b = if b = 0 then 1 else a*f3 a (b-1)
  in f2 ( f3 (f n1) (f n2))