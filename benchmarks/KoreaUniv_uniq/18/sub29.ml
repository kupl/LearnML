let rec uniq_sub : 'a -> 'a list -> 'a list -> 'a list
= fun e lst result -> match lst with
  | [] -> result
  | hd::tl ->
    if hd = e then uniq_sub e tl result
    else uniq_sub e tl (result@[hd]);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (uniq_sub hd tl []));;

(*
uniq [5;6;5;4];;
*)