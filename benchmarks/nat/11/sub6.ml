type nat = ZERO | SUCC of nat

let desucc a = 
       match a with
       |ZERO-> raise (Invalid_argument "sigma") 
       |SUCC ZERO -> ZERO
       |SUCC b -> b

let rec natadd(a,b) = 

        match (a,b) with
        |(ZERO,ZERO) -> ZERO
        |(x,ZERO) -> x
        |(ZERO,y) -> y
        |(SUCC ZERO, y) -> SUCC y
        |(x, SUCC ZERO) -> SUCC x

        |(x,y) -> natadd(desucc x, SUCC y)





let rec natmul(a,b) =

        match (a,b) with
        |(ZERO,_) | (_,ZERO) -> ZERO
        |(SUCC ZERO, y) -> y
        |(x, SUCC ZERO) -> x
        
        |(x,y) -> natmul(desucc x, natadd(b,y))

