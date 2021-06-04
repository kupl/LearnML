let rec max (lst : int list) : int =
  match lst with
  | [] -> 0
  | hd :: tl ->
      let getmax (x : int) (y : int) : int = if x > y then x else y in
      getmax hd (max tl)
