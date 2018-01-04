type nat = ZERO
        | SUCC of nat

let rec eval_nat n =
       match n with
       | ZERO -> 0
       | SUCC( n ) -> 1 + eval_nat n 

let rec numToNat num = 
       match num with
       | 0 -> ZERO
       | num -> SUCC( numToNat ( num - 1 ) )

let natadd( n1, n2 ) = numToNat( eval_nat n1 + eval_nat n2 )
let natmul( n1, n2 ) = numToNat( eval_nat n1 * eval_nat n2 ) 
