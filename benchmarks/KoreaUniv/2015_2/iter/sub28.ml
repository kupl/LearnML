let id = fun x -> x
let composite f g = fun x -> g(f x)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> if n=0 then id else composite f (iter(n-1,f))
