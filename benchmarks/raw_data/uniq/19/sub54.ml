let rec uniq : 'a list -> 'a list
= fun lst -> let rec checker a b = match a with
  | [] -> []
  | hd::tl -> if b = hd then checker tl b else hd::(checker tl b) in
  match lst with
    | [] -> []
    | hd::tl -> hd::(checker (uniq tl) hd);;
