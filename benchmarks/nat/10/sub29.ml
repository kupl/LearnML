exception Error
type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) = match nat2 with ZERO -> nat1
|SUCC(nat)-> (natadd ((SUCC(nat1)), nat))
|_-> raise Error

let rec natmul (nat1, nat2) = 
let rec natmul_in (nat1, nat2, nat3) = match nat2 with ZERO -> ZERO
|(SUCC(ZERO)) -> nat1
|(SUCC(nat)) -> natmul_in ((natadd (nat1, nat3)), nat, nat3)
|_-> raise Error in
natmul_in (nat1, nat2, nat1)