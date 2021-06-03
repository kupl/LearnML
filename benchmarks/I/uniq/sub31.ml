let rec _contains lst value = 
  match lst with
    [] -> false
    | hd::tl -> 
      if hd = value then true
      else _contains tl value;;

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    [] -> []
    | hd::tl ->
      if (_contains tl hd) then uniq tl
      else hd::(uniq tl);;
