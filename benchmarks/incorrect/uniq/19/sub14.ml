let rec check_dup a l =
  match l with
    |[] -> true
    |hd::tl ->
      if a = hd then false
      else check_dup a tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> (* TODO *)
  match lst with
    |[] -> []
    |hd::tl -> 
      if check_dup hd tl = true
      then hd::(uniq tl)
      else uniq tl;;