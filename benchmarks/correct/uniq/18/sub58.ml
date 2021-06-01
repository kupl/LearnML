let uniq : 'a list -> 'a list
= fun lst ->  (* TODO *)

  let rec check_dup lst target = 
    match lst with
      | [] -> false
      | hd::tl -> 
        if hd=target then true
        else check_dup tl target
  in
  let rec uniq2 lst result =
    match lst with
      | [] -> result
      | hd::tl ->
        if (check_dup result hd) then uniq2 tl result
        else uniq2 tl (result@[hd])
  in uniq2 lst [];;
  
  
  uniq [5;6;3;0;0;5;4];;