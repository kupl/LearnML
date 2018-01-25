(* problem 5*)

let rec dfact : int -> int
 = fun n ->
 if (n=1 || n=2) then n
 else dfact (n-2) * n;;