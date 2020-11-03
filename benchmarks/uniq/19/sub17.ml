let rec isin a list =
  match list with
    [] -> []
  | hd::tl -> if a = hd then isin a tl else hd::(isin a tl);;

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    [] -> []
  | hd::tl -> hd::(uniq (isin hd tl));;
  
  uniq [5;6;5;4];;
  uniq [1;2;3;4;5;6];;
  uniq [1;3;5;1;3;6];;
  uniq [1;3;5;1;3;3;5];;