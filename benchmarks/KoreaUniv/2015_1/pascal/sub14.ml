(* 2014210036 김낙현*)

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
let rec pascal (x,y) =
  match (x,y) with 
  |(m,n) -> if m=n then 1
	else if m=0 then 1 else if n=0 then 1 else pascal (m-1,n-1) + pascal (m-1,n)
