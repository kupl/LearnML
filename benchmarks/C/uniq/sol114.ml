let rec helper n lst =  
  match lst with
  | [] -> []
  | hd :: tl -> if n = hd then helper n tl else hd :: (helper n tl);;
  
let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with
  | [] -> []
  | hd :: tl -> hd :: helper hd (uniq tl);;
  
uniq [5;7;7;6;5;4;4;4;4;5] ;;