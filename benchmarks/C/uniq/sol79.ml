let rec remove_element a l =
  match l with
    |[] -> []
    |hd::tl -> if a = hd then remove_element a tl else hd::(remove_element a tl)

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with
    |[] -> []
    |hd::tl -> (hd::(uniq(remove_element hd tl)));;


uniq [5;6;5;4];;