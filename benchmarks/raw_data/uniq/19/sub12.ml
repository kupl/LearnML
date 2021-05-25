let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
             | [] -> []
             | hd::tl -> hd::uniq (let rec remove_dup e l = match l with
                                                            | [] -> []
                                                            | hd::tl -> if e = hd
                                                                        then remove_dup e tl
                                                                        else hd::remove_dup e tl
                                   in remove_dup hd tl)
;;
