(*
    컴퓨터 공학부/ 2007-11740 / 억털괴 / Homework 1
*)

//Exercise 2 시그마
exception IllegalSigma

fun sigma(a, b, f) =
case f of _ : int -> int => //f가 int -> int 임을 명시
    if a > b || a < 0 then raise IllegalSigma
    else if a >= b then f(b)
    else (sigma(a + 1, b, f) + f(a))