(*
    ��ǻ�� ���к�/ 2007-11740 / ���б� / Homework 1
*)

//Exercise 2 �ñ׸�
exception IllegalSigma

fun sigma(a, b, f) =
case f of _ : int -> int => //f�� int -> int ���� ���
    if a > b || a < 0 then raise IllegalSigma
    else if a >= b then f(b)
    else (sigma(a + 1, b, f) + f(a))