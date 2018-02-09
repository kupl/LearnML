let rec double f a : ('a -> 'a) -> 'a -> 'a
= fun f a -> f ( f a )