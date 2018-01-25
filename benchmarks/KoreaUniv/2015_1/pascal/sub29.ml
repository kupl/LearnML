(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) -> 
 let rec factorial a = 
   match a with
    0 -> 1
   |_ -> a*factorial(a-1)
 in factorial(x) / (factorial(y)*factorial(x-y))
