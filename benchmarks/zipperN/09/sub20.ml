(*
    컴퓨터 공학부/ 2007-11740 / 억털괴 / Homework 1
*)


//Exercise 5 자연수
type nat = ZERO | SUCC of nat

fun natadd(a, b) =
case b of ZERO => a
| SUCC(k) => natadd(SUCC(a), k)

fun natmul(a, b) =
case b of ZERO => ZERO
| SUCC(k) => natadd(a, natmul(a, k))

(*
    //디버깅을 위한 추가 함수. 위에서는 사용 X
    fun natval n =
    case n of ZERO => 0
    | SUCC(k) => 1 + natval k
*)