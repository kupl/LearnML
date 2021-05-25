(* problem 2*)
type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
  = fun n1 n2 ->
    match n1 with
    | ZERO -> n2
    | SUCC(x) -> (natadd x (SUCC(n2)));;

let rec natmul : nat -> nat -> nat
  = fun n1 n2 ->
    match n1 with 
    | ZERO -> ZERO
    | SUCC ZERO -> n2
    | SUCC x -> natadd (natmul x n2) n2 ;;


let rec natexp : nat -> nat -> nat
  = fun n1 n2 ->
    match n2 with
      | ZERO -> SUCC ZERO
      | SUCC ZERO -> n1
      | SUCC x -> natmul (natexp n1 x) n1;;
