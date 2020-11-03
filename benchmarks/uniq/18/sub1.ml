let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
    [] -> []
    | [a] -> [a]
    | hd::(nx:: _ as tl) ->
      if hd = nx then uniq tl
      else hd::(uniq tl);;
  