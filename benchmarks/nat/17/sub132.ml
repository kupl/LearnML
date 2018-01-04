type nat = ZERO | SUCC of nat
let rec natadd ((an1 : nat), (an2 : nat)) : nat = 
match (an1,an2) with
|(_,ZERO) -> an1
|(ZERO,_) -> an2
|(SUCC ana1, SUCC ana2) -> natadd(SUCC an1, ana2)

let rec natIter ((nA : nat), (nS : nat),(nC : nat)) : nat = 
match (nA,nS,nC) with
|(_,_,ZERO) -> nS
|(ZERO,_,_) -> ZERO
|(_,_,SUCC nC1) -> natIter(nA, (natadd(nA,nS)), nC1)

let natmul ((mn1 : nat),(mn2 : nat)) : nat = 
natIter(mn1,ZERO,mn2)

