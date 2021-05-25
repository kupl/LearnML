let filter f lst =
  List.fold_right (fun x a -> if f x then x :: a else a) lst []
