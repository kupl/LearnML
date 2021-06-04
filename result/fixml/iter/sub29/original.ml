let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> if n = 0 then f else iter (n - 1, f)


let _ = iter (4, fun x -> 6 + x) 0
