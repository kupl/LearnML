let rec sigma : (int -> int) -> int -> int -> int =
 fun f a b -> if b = a then f a else f a + sigma f (a + 1) b
