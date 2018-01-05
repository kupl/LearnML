(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-7
  Homework-# : 1-4
  Excercise-Name : Natural Number
*)

type nat = ZERO | SUCC of nat;;

(*
natadd : nat * nat -> nat
natmul : nat * nat -> nat
*)

let rec natadd (n1, n2) = 
  match (n1,n2) with
  | (ZERO, ZERO) -> ZERO
  | (ZERO, n)
  | (n, ZERO) -> n
  | (SUCC n1, SUCC n2) -> (
    SUCC ( SUCC ( natadd(n1,n2) ) )
  )
;;

let rec natmul (n1, n2) =
  match (n1,n2) with
  | (ZERO, _ ) -> ZERO
  | (_ , ZERO) -> ZERO
  | (SUCC n1', SUCC n2') -> (
     natadd(natmul(n1',n2), n2) 
  )
;;
