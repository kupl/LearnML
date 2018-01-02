let rec f : int list -> int
= fun lst -> 
    match lst with
      | [] -> max_int
      | hd::tl -> let getmin x y = if x < y then x else y in
        getmin hd (f tl);;