(*problem5*)
let rec  dfact : int -> int = fun n ->
if (n<0) then raise (Failure " error : n is smaller than 0")
else if n=0 then 1
else if n=1 then 1
else if n=2 then 2
else n* dfact (n-2);;