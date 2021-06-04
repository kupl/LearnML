type nat =
|ZERO
|SUCC of nat

let natadd  : (nat -> (nat -> nat)) = 
fun n1 n2 -> 
let rec func_add n1 n2  = 
 (match n1 with 
|ZERO -> n2
|SUCC n1' -> SUCC (func_add (n1') (n2)))
 in 
func_add (n1) (n2)

let natmul  : (nat -> (nat -> nat)) = 
fun n1 n2 -> 
let rec func_num n  = 
 (match n with 
|ZERO -> 0
|SUCC n' -> (1 + func_num (n')))
 in 

let rec func_mul a b  = 
 (match b with 
||0 |1  -> a
|_ -> func_mul (natadd (a) (n1)) ((b - 1)))
 in 
func_mul (
 (match n2 with 
|ZERO -> n2
|SUCC n -> n1)) (func_num (n2))
