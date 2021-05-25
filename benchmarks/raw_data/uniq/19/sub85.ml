let rec drop_duplicate = fun a lst ->
  match lst with
    | [] -> []
    | hd::tl -> if a = hd then drop_duplicate a tl else hd::(drop_duplicate a tl)

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    | [] -> []
    | hd::tl -> hd::(uniq(drop_duplicate hd tl)) 
;;

