let rec isin a l =
  match l with
    [] -> []
  | hd::tl -> if a = hd then isin a tl else hd::(isin a tl);;

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    [] -> []
  | hd::tl -> hd::(uniq (isin hd tl));;
