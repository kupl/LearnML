type nat =ZERO | SUCC of nat;;

let rec natadd(natA,natB) =
 match natA,natB with
 |ZERO,ZERO ->ZERO
 |SUCC(n1),ZERO->natA
 |ZERO,SUCC(n2)->natB
 |SUCC(n1),SUCC(n2)->SUCC(natadd(natA,n2));;
   

let rec natmul(natA,natB) =
    match natA,natB with
    |ZERO,ZERO -> ZERO
    |SUCC(n1),ZERO->ZERO
    |ZERO,SUCC(n2)->ZERO
    |SUCC(n1),SUCC(n2) ->natadd (natA, natmul(natA,n2));;


