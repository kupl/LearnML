type nat =
  ZERO
| SUCC of nat

let rec natadd(n1, n2: nat * nat): nat =
  match (n1, n2) with
    (ZERO, _) -> n2
  | (SUCC(n), _) -> natadd(n, SUCC(n2))

let natmul(n1, n2: nat * nat): nat =
  let rec natmulInternal n1 result =
    match n1 with
      ZERO -> result
    | SUCC(n) -> natmulInternal n (natadd(n2, result))
  in natmulInternal n1 ZERO
