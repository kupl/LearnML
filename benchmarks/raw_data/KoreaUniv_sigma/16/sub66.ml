let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> let rec rsigma a b =
if a > b then 0 else f a + rsigma (a+1) b in
rsigma a b;;


