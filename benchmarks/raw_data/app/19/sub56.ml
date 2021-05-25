let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec append_func l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> insert hd (append_func tl l2)
  and 
  insert a lst =
      match lst with
      | [] -> [a]
      | hd::tl ->
        if hd > a
        then a::hd::tl
        else 
          if hd = a
          then hd::tl
          else hd::(insert a tl)
  in append_func l1 l2;;