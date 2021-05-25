  (* problem 2*)
  type nat = ZERO | SUCC of nat

  let rec natadd : nat -> nat -> nat 
  = fun n1 n2 ->
  match n2 with
  |ZERO -> let rec nat_ev n =
            match n with
            |ZERO -> ZERO
            |SUCC (n2) -> SUCC (nat_ev n2)
            in nat_ev n1
  |SUCC (n3) -> SUCC (natadd n1 n3);;

  let rec natmul : nat -> nat -> nat 
  = fun n1 n2 ->
   match n2 with
   |ZERO -> ZERO
   |SUCC (n3) -> natadd n1 (natmul n1 n3);;


  let rec natexp : nat -> nat -> nat 
  = fun n1 n2 -> 
    match n2 with
    |ZERO -> SUCC ZERO
    |SUCC(n3)->natmul n1 (natexp n1 n3);;
