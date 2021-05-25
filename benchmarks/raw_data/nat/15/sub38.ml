type nat = ZERO | SUCC of nat 


let rec natadd nat1 nat2 =  
match nat1 with
ZERO -> nat2
| SUCC nat3 -> SUCC (natadd nat3 nat2);;

let rec natmul nat1 nat2 = 
match (nat1, nat2) with
(ZERO,_)->ZERO
|(_,ZERO)->ZERO
|(SUCC ZERO,_) -> nat2
|(_, SUCC ZERO) -> nat1
|(SUCC nat3, nat2) -> natadd(natmul nat3 nat2) nat2;;


 