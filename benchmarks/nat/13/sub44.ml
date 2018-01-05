type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
  match n2 with
  | ZERO -> n1
  | SUCC n -> natadd(SUCC n1, n)

let natmul (n1, n2) =
  let rec aux (nat1, nat2) =
    match nat2 with
    | ZERO -> ZERO
    | SUCC ZERO -> nat1
    | SUCC n -> aux (natadd(nat1, n1), n) in
  aux (n1, n2)
