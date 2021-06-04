let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> match n with 1 -> f | _ -> iter (n - 1, f)
