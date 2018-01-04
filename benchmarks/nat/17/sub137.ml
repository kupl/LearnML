type nat = ZERO
         | SUCC of nat

let natadd : nat * nat -> nat = fun (first,second) ->
  let i : nat = ZERO in
  let count : nat -> nat  = fun a -> SUCC of a in
  let rec add : nat * nat -> nat = fun (first, i) ->
    if i == second then first
    else add(count first, count i)

