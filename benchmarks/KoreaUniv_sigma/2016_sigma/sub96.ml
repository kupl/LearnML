let is_enough x y = if x>y then true else false;;
let sum f x y = let x = (f) x in x+y;;
let rec sigmaloop f x y z = if is_enough x y then z else sigmaloop f (x+1) y (sum f x z);;
let sigma f x y = sigmaloop f x y 0;;
