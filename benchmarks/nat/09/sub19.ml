(*
    ��ǻ�� ���к�/ 2007-11740 / ���б� / Homework 1
*)


//Exercise 6 2ģ��
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

fun crazy2val n =
case n of NIL => 0
| ZERO(k) => 2 * crazy2val k
| ONE(k)  => 1 + 2 * crazy2val k
| MONE(k) => -1 + 2 * crazy2val k

    | SUCC(k) => 1 + natval k
*)