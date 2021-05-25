let rec checker n l = match l with
  | [] -> []
  | hd::tl -> if (n=hd) then checker n tl else hd::(checker n tl)

let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  | [] -> []
  | hd::tl -> hd::uniq (checker hd tl);;
  