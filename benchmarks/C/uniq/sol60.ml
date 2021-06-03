let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    | [] -> []
    | h::t -> h::(uniq (List.filter (fun x -> x<>h) t))
;;

