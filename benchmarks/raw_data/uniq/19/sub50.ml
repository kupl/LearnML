let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec deldup e lst =
    match lst with
      | [] -> []
      | hd::tl -> if hd=e then deldup e tl else hd::(deldup e tl) in
  match lst with
    | [] -> []
    | hd::tl -> hd:: uniq (deldup hd tl);;
    
uniq [5;6;5;4];;