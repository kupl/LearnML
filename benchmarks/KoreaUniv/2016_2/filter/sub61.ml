let rec filter pred lst
= List.fold_right (fun x a -> if pred x then x :: a else a) lst []
