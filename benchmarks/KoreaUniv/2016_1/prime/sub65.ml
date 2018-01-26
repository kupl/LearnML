let rec isprime : int * int -> bool
= fun (n,d) -> 
if d = 1 then true
else if n mod d = 0 then false
else isprime (n, d-1);;

let rec prime : int -> bool
= fun n -> if n = 1 then false else isprime (n, n-1);;
