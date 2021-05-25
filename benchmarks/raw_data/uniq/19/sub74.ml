let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec uniq_help l n = 
    match l with
      | [] -> []
      | h :: t -> if n = h then uniq_help t n else h::(uniq_help t n) 
  in
  match lst with
    | [] -> []
    | h::t -> h::(uniq_help (uniq t) h);;

uniq [5;6;5;4];;