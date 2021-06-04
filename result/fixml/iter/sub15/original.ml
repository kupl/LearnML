let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) k -> match k with 0 -> f n | t -> iter (f n, f) (k - 1)
