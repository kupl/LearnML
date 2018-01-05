(* 컴퓨터공학과/2017-34165/김성국/1-4 *)
type nat = ZERO | SUCC of nat

(*
let zero = ZERO
and one = SUCC(ZERO)
and two = SUCC(SUCC(ZERO))
and three = SUCC(SUCC(SUCC(ZERO)))
and four = SUCC(SUCC(SUCC(SUCC(ZERO))))
and five = SUCC(SUCC(SUCC(SUCC(SUCC(ZERO)))))
*)

let rec natadd (x, y) =
  match (x, y) with
  | (ZERO, y) -> y
  | (x, ZERO) -> x
  | (SUCC xp, y) -> SUCC(natadd (xp, y))
and natmul (x, y) =
  match (x, y) with
  | (ZERO, y) -> ZERO
  | (x, ZERO) -> ZERO
  | (SUCC xp, y) -> natadd (y, (natmul (xp, y)))
