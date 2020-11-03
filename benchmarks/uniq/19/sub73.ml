let uniq : 'a list -> 'a list
= fun lst -> List.sort_uniq compare lst;;

uniq [5;6;5;4];;