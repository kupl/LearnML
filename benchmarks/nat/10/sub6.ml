(* ��ǻ�Ͱ��к� / 2005-11721 / ����� / ����1-6 *)
type nat = ZERO | SUCC of nat
let rec natadd(n1,n2)=
    match n1 with
      ZERO -> n2
    | SUCC(n1_sub) -> SUCC(natadd(n1_sub,n2))
let rec natmul(n1,n2)=
    match n1 with
      ZERO -> ZERO
    | SUCC(n1_sub) -> natadd(n2,natmul(n1_sub,n2))