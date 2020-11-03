let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let mergedList = List.rev_append (List.rev l1) l2 in
  List.sort_uniq compare mergedList ;;


app [2;3;4;5] [5;2;10];;