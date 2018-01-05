type nat = ZERO | SUCC of nat
 
let rec natadd (n1, n2) =
  match n1 with
  | ZERO  -> n2
  | SUCC n1d->SUCC (natadd (n1d, n2))
 
let rec natmul (n1, n2) =
  match n1 with
  | ZERO -> ZERO
  | SUCC n1d -> natadd (natmul (n1d, n2), n2)