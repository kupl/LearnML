let rec f1 l m=
  match l with
    | [] -> []
    | h::t -> if h = m then (f1 t m) else h::(f1 t m)
;;


let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l2 with
    | [] -> l1
    | h2::t2 -> h2::(app (f1 l1 h2) t2) 
;;
