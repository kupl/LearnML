let rec iter ((n: int), (f: 'a -> 'a)) (x: 'a) : 'a =
     match n with
     | 0 -> x
     | n -> iter (n-1, f) (f x)
