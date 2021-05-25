let rec rmv_lst lst num = 
  match lst with
  | [] -> []
  | hd::tl -> if ( hd = num ) then rmv_lst tl num else hd::( rmv_lst tl num );;

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with
    | [] -> []
    | hd::tl -> hd::uniq ( rmv_lst tl hd);;
    
