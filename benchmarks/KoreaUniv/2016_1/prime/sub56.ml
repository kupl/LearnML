let rec myprime : int * int -> bool
= fun (n1,n2) -> if n2=1 then true else if n1 = n2*(n1/n2) then false else myprime(n1,n2-1)
let rec prime : int -> bool
= fun n -> myprime(n,n/2)
