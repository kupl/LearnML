(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> let rec loop n = match n with 
0 -> 1
| _ -> if n mod 2 = 0 then (fun x -> x*x) (loop (n/2))
else b*(loop (n-1))
in (loop n)
