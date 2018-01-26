let rec prime : int -> bool
= fun n ->
if n = 0 then false else
if n = 1 then false else
let rec div (n,a) =
if (n mod a <> 0) && a>1 then div (n,a-1) else
if a = 1 then true else false
in div (n,n-1);;
