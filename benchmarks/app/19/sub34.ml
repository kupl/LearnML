let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
      match l1 with
        | hd::tl -> let rec exist = fun lst a -> match lst with
                                                | hd::tl -> if hd=a then true else exist tl a
                                                | [] -> false
                    in if exist l2 hd then app tl l2 else app tl (l2@[hd])
        | [] -> l2;;
        
        app [4;5;6;7] [1;2;3;4];;