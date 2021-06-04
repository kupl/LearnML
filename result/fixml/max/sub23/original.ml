let rec fold f l a = match l with [] -> a | hd :: tl -> f hd (fold f tl a)

let max lst = fold (fun x y -> if x >= y then x else y) lst 0
