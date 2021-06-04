let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> if n > 0 then iter (n - 1, fun x -> f x) else fun x -> x


let _ = iter (3, fun x -> 2 + x) 0
