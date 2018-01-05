type nat =
  | ZERO
  | SUCC of nat

let rec nataddr (a,b) : int =
  match (a,b) with
    | (ZERO,ZERO) -> 0
    | (ZERO,SUCC c) -> 1 + nataddr (ZERO,c)
    | (SUCC f,ZERO) -> 1 + nataddr (f,ZERO)
    | (SUCC d,SUCC e) -> 2 + nataddr (d,e)

let rec nattoint a : int =
  match a with
    | ZERO -> 0
    | SUCC b -> 1 + nattoint b

let rec inttonat (a:int) : nat =
  match a with
    | 0 -> ZERO
    | _ -> SUCC (inttonat (a-1))

let rec natadd (a,b) : nat =
  inttonat (nataddr (a,b))

let rec natmul (a,b) : nat =
  inttonat ((nattoint a) * (nattoint b))