let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) x -> if n = 1 || n = 0 then f x else f (iter (n - 1, f) x)
