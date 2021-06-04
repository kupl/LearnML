let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> if n = 1 then f else fun x -> f (iter (n - 1, f) x)
