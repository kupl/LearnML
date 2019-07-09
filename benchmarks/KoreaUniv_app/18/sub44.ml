let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec uniq : 'a list -> 'a list
  = fun lst ->
    let rec rida : 'a*'a list -> 'a list
    = fun (elem, alist) ->
      List.filter (fun x -> x != elem) alist in
    match lst with
      | [] -> []
      | x::xs -> x::uniq (rida (x, xs))
  in uniq (l2@l1);;