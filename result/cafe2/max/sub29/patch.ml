let bigger (x : int) (y : int) : int = if x > y then x else y

let rec fold (bigger : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with
  | [] -> a
  | [ __s9 ] -> __s9
  | hd :: tl -> bigger hd (fold bigger tl a)


let rec max (lst : int list) : int = fold bigger lst 0
