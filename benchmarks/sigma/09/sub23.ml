exception Error of string

let rec sigma = function (x, y, f) -> let sum = List.fold_left (+) 0 in let rec range a b = if a > b then [] else a :: range(a+1) b in if x > y then raise(Error "Invalid arg") else sum (List.map (f) (range x y));;