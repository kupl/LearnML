(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with
| (_,1) -> 1
| (n,k) -> if n=k then 1 else pascal (n-1,k-1) + pascal (n-1, k);;
