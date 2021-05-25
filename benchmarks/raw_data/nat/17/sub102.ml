(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 ->
  match n1 with
  | ZERO -> n2
  | SUCC(temp) ->
    SUCC(natadd temp n2)

let rec natmul1 : nat -> nat -> nat -> nat
= fun n1 n2 n22 ->
  match n1 with
  |ZERO -> ZERO
  |SUCC(newn1) ->
    (match n2 with
     |ZERO -> natmul1 newn1 n22 n22
     |SUCC(newn2)->
      SUCC(natmul1 n1 newn2 n22)
    )

let rec natmul : nat -> nat -> nat 
= fun n1 n2 ->
  natmul1 n1 n2 n2

let rec natexp1 : nat-> nat -> nat -> nat
= fun n1 n11 n2 ->
  match n2 with
  |ZERO -> n1
  |SUCC(newn2) ->
    let newn1 = natmul n1 n11 in
    natexp1 newn1 n11 newn2

let rec natexp : nat -> nat -> nat 
= fun n1 n2 ->
  match n2 with
  |ZERO -> (SUCC ZERO)
  |SUCC(newn2) -> natexp1 n1 n1 newn2