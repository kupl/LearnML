(*
    ��ǻ�� ���к�/ 2007-11740 / ���б� / Homework 1
*)


//Exercise 3 �ݺ���
exception IllegalIterFunc

fun iter(n, f) =
case f of _ : int -> int => //f �� int -> int ���� ���
if n < 0 then raise IllegalIterFunc
else if n = 0 then fn x => x
else fn x => f(iter(n - 1, f) x)