let rec __s1 (__s2 : int -> int) (__s3 : int -> int) (__s4 : int) : int =
  __s2 (__s3 __s4)


let iter ((n : int), (f : int -> int)) : int -> int =
  let idf (n : int) : int = n in

  let rec help (n : int) (f : int -> int) : int -> int =
    if n = 0 then idf else __s1 f (help (n - 1) f)
  in
  help n f
