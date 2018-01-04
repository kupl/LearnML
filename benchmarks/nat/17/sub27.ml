type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = fun (a, b) ->
  match a with
  | ZERO -> b
  | SUCC c -> natadd (c, SUCC b)

let natmul : nat * nat -> nat = fun (a, b) ->
  let rec inadd : nat * nat -> nat = fun (a, b) ->
    match a with
    | ZERO -> b
    | SUCC c -> inadd (c, SUCC b) in
  let rec inmul : nat * nat * nat -> nat = fun (a, b, r) ->
    match a with
    | ZERO -> r
    | SUCC c -> inmul (c, b, inadd(b, r)) in
  if(a == ZERO || b == ZERO) then ZERO
  else inmul (a, b, ZERO)
