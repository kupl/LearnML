let filter f l =
List.fold_right (fun x a -> if f x then x :: a else a) l []
