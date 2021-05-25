let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec f lst1 lst2 =
    match lst1 with
      | [] -> lst2
      | hd::tl -> let rec g a lst =
                    match lst with
                      | [] -> true
                      | hd::tl ->
                        if hd = a then false
                        else g a tl
                  in
                    if (g hd lst2) then f tl (lst2@[hd])
                    else f tl lst2
  in f l1 l2;;
