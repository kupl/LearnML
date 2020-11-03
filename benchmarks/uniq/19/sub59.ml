let rec uniq : 'a list -> 'a list
  = fun l ->
    let rec remove_all l a =
      match l with
      | [] -> []
      | h :: t -> if h = a then
          (remove_all t a)
        else
          h :: (remove_all t a)
    in match l with
    | [] -> []
    | h :: t ->
      h :: (uniq (remove_all t h));;

uniq [5;6;5;4];;
uniq [1;2;3;4;5;5;4;3;2;1];;
uniq [4;3;2;1;5;5;1;2;3;4];;
uniq [4;3;3;2;2;1;5;1;5];;
