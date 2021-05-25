let rec compare n lst = match lst with
  | [] -> []
  | hd::tl -> if n = hd then compare n tl else hd::(compare n tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (compare hd tl));;

uniq [5;6;5;4;1;5;3;6;34;5;3;4;2;6;3;4;6;3;2;3;4;2;1];;