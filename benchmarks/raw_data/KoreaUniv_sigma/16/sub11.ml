let rec sigma : (int -> int) -> int -> int -> int
= fun f n1 n2 -> if n1<=n2 then f(n1) + sigma f(n1+1) n2 else 0;;