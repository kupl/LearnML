exception Error of string;;

type nat = ZERO | SUCC of nat;;

let rec natadd n1 n2 =
  match(n1, n2) with
      (ZERO, ZERO) -> ZERO
    | (ZERO, SUCC a) -> (SUCC a)
    | (SUCC a, ZERO) -> (SUCC a)
    | (SUCC a, SUCC b) -> (natadd (SUCC (SUCC a)) b);;

let rec natmul2 n1 n2 n =
  match(n1, n2) with
      (ZERO, ZERO) -> ZERO
    | (ZERO, SUCC a) -> ZERO
    | (SUCC a, ZERO) -> ZERO
    | (SUCC ZERO, SUCC a) -> (SUCC a)
    | (SUCC a, SUCC ZERO) -> (SUCC a)
    | (SUCC a, SUCC b) -> (natmul2 (natadd n1 n) b n);;


let natmul n1 n2 =
  (natmul2 n1 n2 n1);; 
