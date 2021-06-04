let rec max : int list -> int =
 fun lst ->
  match lst with
  | [] -> 0
  | hd :: tl ->
      let getmax x y = if x > y then x else if tl = [] then x else y in
      getmax hd (max tl)
