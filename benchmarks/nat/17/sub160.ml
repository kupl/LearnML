type nat = ZERO | SUCC of nat

let rec natadd : (nat * nat -> nat) = function(x, y) -> match x with
  | ZERO -> y
  | SUCC x' -> (natadd(x', SUCC y))

let rec natmul : (nat * nat -> nat) = function(x, y) -> (
  let rec natmul2 (x : nat) (y : nat) (z : nat) : nat = (
    match x with
      | ZERO -> z
      | SUCC x' -> natmul2 x' y (natadd(y, z))
  ) in natmul2 x y ZERO
)
