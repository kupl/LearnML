let iter ((n : int), (f : int -> int)) : int -> int =
  let idf (n : int) : int = n in

  let rec help (n : int) (f : int -> int) : int -> int =
    if n = 0 then idf else if n = 1 then f else f
  in
  help n f
