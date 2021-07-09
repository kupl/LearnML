let rec iter ((n : int), (f : int -> int)) : int -> int =
  match n with
  | 0 -> fun (__s5 : int) -> __s5
  | 1 -> f
  | _ -> fun (x : int) -> f (iter (n - 1, f) x)


let (_ : int) = iter (2, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (4, fun (x : int) -> 2 + x) 0

let (_ : int) = iter (2, fun (x : int) -> 2 + x) 1

let (_ : int) = iter (3, fun (x : int) -> 2 + x) 1

let (_ : int) = iter (4, fun (x : int) -> 2 + x) 1
