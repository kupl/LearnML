let uniq : 'a list -> 'a list
= fun lst ->
  let aa = fun acc x -> (if List.exists ((=) x) acc then acc else x :: acc) in
  List.rev(List.fold_left aa [] lst)
;;
  
uniq [5;6;5;4];;
