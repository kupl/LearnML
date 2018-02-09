let rec filter pred lst = fold (fun x y -> if pred x then x::y
else y) lst []
