let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec remove a l = 
    match l with
    | [] -> []
    | hd::tl -> if hd = a then (remove a tl) else hd::(remove a tl)
  in match lst with
  | [] -> []
  | hd::tl -> hd::(uniq (remove hd tl));;