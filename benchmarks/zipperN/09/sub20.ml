(*
    ��ǻ�� ���к�/ 2007-11740 / ���б� / Homework 1
*)


//Exercise 5 �ڿ���
type nat = ZERO | SUCC of nat

fun natadd(a, b) =
case b of ZERO => a
| SUCC(k) => natadd(SUCC(a), k)

fun natmul(a, b) =
case b of ZERO => ZERO
| SUCC(k) => natadd(a, natmul(a, k))

(*
    //������� ���� �߰� �Լ�. �������� ��� X
    fun natval n =
    case n of ZERO => 0
    | SUCC(k) => 1 + natval k
*)