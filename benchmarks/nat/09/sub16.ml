type nat = ZERO | SUCC of nat

let rec natadd (a,b)=
match a with
ZERO -> b |
SUCC(a2) -> natadd (a2,(SUCC b))

let rec natmul (a,b)=
match a with
ZERO -> ZERO |
SUCC(a2) -> natadd (b,(natmul (a2,b)))