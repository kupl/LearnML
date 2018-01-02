let rec f : int list -> int
= fun lst ->
      match lst with
        | [] -> 0
        | hd::tl -> let getmax x y = if x > y then x else y in
          getmax hd (f tl);;


