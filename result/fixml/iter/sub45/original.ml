let compose f g x = f (g x)

let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> match n with 0 -> f | 1 -> f | _ -> compose f (iter (n - 1, f))
