let rec iter : int * (int -> int) -> int -> int =
 fun (n, f) -> match n with 1 -> f | _ -> fun x -> f (iter (n - 1, f) x)


let _ = iter (2, fun x -> 2 + x) 0

let _ = iter (3, fun x -> 2 + x) 0

let _ = iter (4, fun x -> 2 + x) 0

let _ = iter (2, fun x -> 2 + x) 1

let _ = iter (3, fun x -> 2 + x) 1

let _ = iter (4, fun x -> 2 + x) 1
