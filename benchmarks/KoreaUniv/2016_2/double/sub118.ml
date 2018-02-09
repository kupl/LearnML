let rec double f a  (* (’a -> ’a) -> ’a -> ’a *)
  = f (f a)
