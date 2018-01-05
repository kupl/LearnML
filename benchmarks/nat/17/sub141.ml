type nat = ZERO | SUCC of nat

let rec natadd: nat * nat -> nat = fun (a, b) ->
  match a with
  | ZERO -> b
  | SUCC k ->
    (match b with
    | ZERO -> a
    | SUCC _ -> SUCC (natadd (k, b)))

let rec natmul: nat * nat -> nat = fun (a, b) ->
  match a with
  | ZERO -> ZERO
  | SUCC k ->
    (match b with
    | ZERO -> ZERO
    | SUCC _ -> natadd (natadd (b, ZERO), natmul (k, b)))
