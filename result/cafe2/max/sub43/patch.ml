let rec fold (f : 'b -> 'a -> 'a) (l : 'b list) (a : int) =
  match l with [] -> a | hd :: tl -> fold f tl (f hd a)


let rec max (l : int list) : int =
  let a : int = -1073741824 in
  fold
    (fun (__s7 : int) (__s8 : int) -> if __s8 > __s7 then __s8 else __s7)
    l min_int
