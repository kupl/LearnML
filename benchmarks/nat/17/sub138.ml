type nat = ZERO
         | SUCC of nat

let natadd : nat * nat -> nat = fun (first, second) ->
  let i : nat = ZERO in
  let rec add : nat * nat -> nat = fun (a, b) -> (
    if b = second then a
    else add(SUCC a, SUCC b)) in
  add(first, i)

let natmul : nat * nat -> nat = fun (first, second) ->
  let i : nat = ZERO in
  let rec mul : nat * nat -> nat = fun (a, b) -> (
    if b = second then a
    else mul(natadd(a,first), SUCC b)) in
  mul(ZERO, i)
