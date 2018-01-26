let rec prime : int -> bool
= fun n -> if n=1 then false else let rec divide m
= if n=m then true else if (n mod m) = 0 then false else divide (m+1)
in divide 2;; (* TODO *)
