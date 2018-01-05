type nat= ZERO|SUCC of nat


let rec natadd(x, y)=
match (x, y) with
|(a, ZERO)->a
|(a, SUCC b)->natadd(SUCC a, b)

let rec natmul(x, y)=
match (x,y) with
|(a, ZERO) -> ZERO
|(a, SUCC b)-> natadd(natmul(a, b), a) 
