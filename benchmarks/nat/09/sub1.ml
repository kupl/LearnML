type nat = ZERO | SUCC of nat

let rec natadd ((x : nat), (y : nat)) =
    let makenat (x : nat) = SUCC x
    in
    
    match y with
       ZERO -> x
     |SUCC(f) -> natadd (makenat(x),f) 

let rec natmul ((x:nat),(y:nat)) =
    
    match (x,y) with
       (ZERO, b) -> ZERO
     | (a, ZERO) -> ZERO
     |(SUCC(ZERO), b) -> b
     |(SUCC(a),b) -> natmul(a, natadd(b,b));;