(*
    컴퓨터 공학부/ 2007-11740 / 억털괴 / Homework 1
*)


//Exercise 3 반복기
exception IllegalIterFunc

fun iter(n, f) =
case f of _ : int -> int => //f 가 int -> int 임을 명시
if n < 0 then raise IllegalIterFunc
else if n = 0 then fn x => x
else fn x => f(iter(n - 1, f) x)