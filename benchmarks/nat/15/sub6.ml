type nat = ZERO | SUCC of nat

let rec natadd : nat*nat -> nat = fun ((n1:nat),(n2:nat)) ->
    match (n1,n2) with
    |(ZERO,ZERO) -> ZERO
    |(n1,ZERO) -> n1
    |(ZERO,n2) -> n2
    |(SUCC s1, n2) -> SUCC (natadd(s1, n2));;

let rec natmul : nat*nat -> nat = fun ((n1:nat),(n2:nat)) ->
    match (n1,n2) with
    |(_,ZERO) -> ZERO
    |(ZERO,_) -> ZERO
    |(SUCC s1, n2) -> natadd(natmul(s1,n2),n2);;


