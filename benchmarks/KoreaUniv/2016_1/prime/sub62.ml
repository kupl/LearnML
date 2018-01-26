let rec prime : int -> bool
= fun n ->
let rec isprime : int -> int ->bool
= fun n a -> if a >= n then true else if a < n && n mod a = 0 then false else isprime n (a + 1) in
if n <= 1 then false else isprime n 2
