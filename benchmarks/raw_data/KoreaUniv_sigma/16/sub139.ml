let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
let rec sigma2 f n s =
if n<=b then sigma2 f (n+1) (s+f n)
else s
in sigma2 f a 0;;
