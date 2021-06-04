let who_is_G x y = if x > y then x else y

let rec fold f l a =
  match l with
  | [] -> 1
  | hd :: tl -> if tl = [] then fun __x__ -> hd else who_is_G hd (fold f tl a)


let max lst = fold who_is_G lst 1
