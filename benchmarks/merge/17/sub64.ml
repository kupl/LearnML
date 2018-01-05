let rec merge (x : int list * int list) : int list =
let (a, b) = x in
if b == [] then a
else if a == [] then b
else if (List.hd a) > (List.hd b) then (List.hd a)::merge(List.tl a, b)
else (List.hd b)::merge(a, List.tl b)
