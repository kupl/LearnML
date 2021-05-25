let rec uniq : 'a list -> 'a list
= fun lst -> 
  let lst2 = [] in
  let rec sub_uniq lst lst2 = 
  match lst with
    |[] -> lst2
    |head :: tail ->
      let rec is_dup lst x =
        match lst with
          |[] -> false
          |head2::tail2 -> 
            if x = head2 then true
            else is_dup tail2 x in
              if is_dup lst2 head = false then sub_uniq tail (lst2 @ [head])
              else sub_uniq tail lst2
                in sub_uniq lst lst2;;
          