type nat = ZERO | SUCC of nat

let rec natadd (n1,n2) = 
    match (n1,n2) with
    (ZERO,b) -> b
    |(a,ZERO) -> a
    |(x,SUCC y) -> natadd(SUCC x,y)
    
let rec natmul (n1,n2) =
    
