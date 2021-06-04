let rec fold f l a = match l with [] -> a | hd :: tl -> fold f tl (f hd a)

let rec max l =
  let a = -1073741824 in
  fold (fun x y -> if x > y then x else if y = a then x else y) l a
