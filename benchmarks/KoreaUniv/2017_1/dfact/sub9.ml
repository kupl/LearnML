(* problem 5*)

let rec dfact : int -> int
= fun n ->
if n<0 then raise (Invalid_argument "dfact")
else if n=1||n=0 then 1
else n*(dfact (n-2))