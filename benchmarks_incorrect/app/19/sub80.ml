let rec helper n lst =  
  match lst with
  | [] -> []
  | hd :: tl -> if n = hd then helper n tl else hd :: (helper n tl);;
  
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  match l2 with
  | [] -> l1
  | hd::tl -> hd :: helper hd (app l1 tl);;
  
app [4;5;6;7] [1;2;3;4;4;6;8] ;;