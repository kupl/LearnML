let rec sigma (f : int -> int) (a : int) b : int =
  match a with b -> f a | _ -> sigma f (a + 1) b + f a
