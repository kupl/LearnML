(* Problem 3 *)
let rec iter : int * (int -> int) -> (int -> int)
  = fun (n,f) -> 
    if n>0
    then (fun x -> (let f2 = iter (n-1,f) in f2(f x)))
    else fun x -> x;;