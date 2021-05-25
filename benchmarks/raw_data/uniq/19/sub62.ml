let uniq : 'a list -> 'a list
= fun lst -> 
  let rec f (lst1 : 'a list) (lst2 : 'a list) =
    match lst2 with
      | [] -> lst1
      | hd::tl -> let rec g a l =
                    match l with
                      | [] -> true
                      | hd::tl ->
                        if hd = a then false
                        else g a tl
                  in
                    if (g hd lst1) then f (lst1@[hd]) tl
                    else f lst1 tl
  in f [] lst;;
