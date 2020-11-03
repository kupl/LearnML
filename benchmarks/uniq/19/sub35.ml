let uniq : 'a list -> 'a list
= fun lst -> (* TODO *);;
     
     let rec drop_value l to drop=
     match l with 
     | [] -> []
     | hd::tl ->
       let new tl= drop_value tl to_drop in
        hd=to_drop 