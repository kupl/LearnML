let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (search hd tl))

and search : 'a -> 'a list -> 'a list
= fun s l -> match l with
  | [] -> []
  | hd::tl -> if s = hd then tl else hd::(search s tl);;
  
uniq [5;6;5;4];;