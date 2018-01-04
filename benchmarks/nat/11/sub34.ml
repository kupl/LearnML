type nat = ZERO | SUCC of nat
let rec natadd(x,y)=match x with
        |ZERO->y
        |SUCC(n)->natadd(n,SUCC(y))
let rec natmul(x,y)=match x with
        |ZERO->ZERO
        |SUCC(n)->natadd(natmul(n,y),y)
