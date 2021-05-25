let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
          match lst with
            | hd::tl -> let rec delete_others = fun lst a -> 
                                                          match lst with
                                                            | hd::tl -> if hd=a then delete_others tl a else [hd]@delete_others tl a
                                                            | [] -> []
                        in [hd]@uniq (delete_others tl hd)
            | [] -> [];;
            
            uniq [5;6;5;4];;