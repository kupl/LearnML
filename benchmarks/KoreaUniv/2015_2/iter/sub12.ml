let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> let returnf : int -> int = fun x -> x in returnf
| 1 -> f
| n -> f (fun (n-1,f));;
