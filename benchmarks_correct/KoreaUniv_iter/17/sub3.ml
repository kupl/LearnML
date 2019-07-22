  (*Problem 3*)
let rec iter(n,(f:int -> int)) = 
match n with
|0 -> (fun x -> x)
|1 -> f
|_ -> (fun x -> f ((iter (n-1, f) x)));;