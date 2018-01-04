(*컴공 2014-10618 이세영 1-5*)
type nat=ZERO | SUCC of nat;;
let rec natadd (x, y)=
    match x with
    |ZERO->y
    |SUCC t->SUCC(natadd(t,y))
;;
let rec natmul (x, y)=
    match y with
    |ZERO->ZERO
    |SUCC t->natadd(x,natmul(x,t))
;;
