type nat = ZERO | SUCC of nat

let succ (a: nat): nat =
  match a with
  | a -> SUCC a

let pred (a: nat): nat =
  match a with
  | ZERO -> ZERO
  | SUCC b -> b

let rec iter ((n: nat), (f: nat -> nat)) (param: nat): nat =
  match (n, f) with
  | (ZERO, f) -> param
  | (n, f) -> f (iter(pred n, f) param)


let rec natadd ((a: nat), (b: nat)): nat = 
  match (a, b) with
  | (ZERO, b) -> b
  | (a, ZERO) -> a
  | (a, b) -> natadd (succ a, pred b)

let rec natmul ((a: nat), (b: nat)): nat =
  match (a, b) with
  | (ZERO, b) -> ZERO
  | (a, ZERO) -> ZERO
  | (SUCC ZERO, b) -> b
  | (a, SUCC ZERO) -> a
  | (a, b) -> natmul (iter(b, succ) a, pred b)
