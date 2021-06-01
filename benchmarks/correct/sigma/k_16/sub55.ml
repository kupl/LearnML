let rec sigma : (int -> int) -> int -> int -> int
= fun f p q -> if p = q then (f p) else (f p) + (sigma f (p+1) q);;