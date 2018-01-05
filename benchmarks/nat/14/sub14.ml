(* 프로그래밍언어 Homework1 2009-11657 김동현 *)

(* Exercise 3 *)
type nat = ZERO | SUCC of nat

let aux3 fm =
  match fm with
    ZERO -> ZERO
  | SUCC a -> a

let rec natadd (a, b) =
  if a = ZERO then
    if b = ZERO then ZERO
    else b
  else 
    if b = ZERO then a
    else SUCC (SUCC (natadd (aux3 a, aux3 b)))

let rec natmul (a, b) =
  if a = ZERO then ZERO
  else
    if b = ZERO then ZERO
    else natadd (a, natmul (a, aux3 b))
