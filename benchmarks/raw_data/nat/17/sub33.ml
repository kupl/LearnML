(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC x -> natadd x (SUCC n2)

let natmul : nat -> nat -> nat
= fun n1 n2 ->
  let rec loop = fun cnt acc ->
    match cnt with
    | ZERO -> acc
    | SUCC x -> loop x (natadd acc n2)
  in
  loop n1 ZERO

let natexp : nat -> nat -> nat
= fun n1 n2 ->
  match n1 with
  | ZERO -> ZERO
  | _ ->
    let rec loop = fun cnt acc ->
      match cnt with
      | ZERO -> acc
      | SUCC x -> loop x (natmul acc n1)
    in
    loop n2 (SUCC ZERO)
