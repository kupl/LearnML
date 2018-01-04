type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
  match n1, n2 with
  | ZERO, ZERO -> ZERO
  | SUCC n, _ ->
    SUCC (natadd (n,n2))
  | _, SUCC n ->
    SUCC (natadd (n1, n))

let rec natmul (n1, n2) =
  let rec natmul' (n1, n2) acc =
    match n1 with
    | ZERO -> acc
    | SUCC z ->
      natmul' (z, n2) (natadd (n2, acc))
  in
  natmul' (n1, n2) ZERO
