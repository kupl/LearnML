let __s6 (__s7 : int -> int) (__s8 : int -> int) (__s9 : int) : int =
  __s7 (__s8 __s9)


let iter ((n : int), (f : int -> int)) : int -> int =
  let idf (n : int) : int = n in

  let rec help (n : int) (f : int -> int) : int -> int =
    if n = 0 then idf else __s6 f (help (n - 1) f)
  in
  help n f
