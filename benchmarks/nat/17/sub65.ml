type nat = ZERO | SUCC of nat

let rec natadd : (nat * nat) -> nat = fun (a, b) ->
  match a with 
  | ZERO -> b
  | SUCC n -> (SUCC (natadd (n, b)))

let rec natmul : (nat * nat) -> nat = fun (a, b) ->
  match b with
  | ZERO -> ZERO
  | SUCC n -> natadd (a, natmul (a, n))

let rec gen : int -> nat = fun a ->
  if (a == 0) then ZERO else SUCC (gen (a-1))

let print_nat : nat -> unit = fun a ->
  let rec add : nat -> int = fun b -> 
    match b with 
    | ZERO -> 0
    | SUCC n -> 1 + add n in
  print_int (add a); print_endline ""
