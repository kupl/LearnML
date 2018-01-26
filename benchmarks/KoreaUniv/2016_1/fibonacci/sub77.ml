(* Problem 1 *)
let rec fib : int -> int
= fun n ->
    if n < 0 then 0 else
    let rec loop n a b = 
        if n = 0 
            then b
            else loop (n - 1) b (a + b) in 
    loop n 1 0
