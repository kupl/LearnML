type nat = ZERO | SUCC of nat 

let rec natadd : nat * nat -> nat = fun (n1, n2) ->
match (n1,n2) with
|(ZERO, ZERO) -> ZERO
|(ZERO, SUCC n) -> SUCC n
|(SUCC n, ZERO) -> SUCC n
|(SUCC n3, SUCC n4) -> SUCC(SUCC(natadd (n3, n4))) 


and natmul : nat*nat->nat = fun (n1,n2) ->
match (n1,n2) with
|(ZERO, ZERO) -> ZERO
|(ZERO, SUCC n) -> ZERO
|(SUCC n, ZERO) -> ZERO
|(SUCC n3, SUCC n4) -> natadd (natmul(n3, SUCC n4), SUCC n4)

