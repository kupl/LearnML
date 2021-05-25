let rec f1 l m=
  match l with
    | [] -> []
    | h::t -> if h = m then (f1 t m) else h::(f1 t m)
;;


let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with
    | [] -> []
    | h::t -> h :: uniq (f1 lst h)
;;
