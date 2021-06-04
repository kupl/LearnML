let bigger x y = if x > y then x else y

let rec fold bigger l a =
  match l with [] -> a | hd :: tl -> bigger hd (fold bigger tl a)


let rec max : int list -> int = fun lst -> fold bigger lst 0
