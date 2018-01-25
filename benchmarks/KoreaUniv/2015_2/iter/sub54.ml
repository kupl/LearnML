let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
if n<0 then raise (Failure "n is smaller than zero")
else(
match n with
| 0 -> (fun x -> x)
| n -> (fun x -> iter(n-1,f) (f x))
)
