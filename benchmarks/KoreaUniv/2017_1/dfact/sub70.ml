(*problem 5*)
let dfact : int -> int
= fun n ->
let rec dfac n =
if n < 2 then 1
else n * dfac (n-2) in dfac n;;