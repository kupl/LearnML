let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> l2@(let rec app_internal l = match l with
                                            | [] -> []
                                            | hd::tl -> let rec isIn lst elem = match lst with
                                                                                | [] -> false
                                                                                | hd::tl -> if hd = elem
                                                                                            then true
                                                                                            else isIn tl elem
                                                        in if isIn l2 hd
                                                           then app_internal tl
                                                           else hd::app_internal tl
                   in app_internal l1)
;;