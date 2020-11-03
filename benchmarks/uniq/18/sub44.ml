let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec rida : 'a*'a list -> 'a list
  = fun (elem, alist) ->
    List.filter (fun x -> x != elem) alist in
  match lst with
    | [] -> []
    | x::xs -> x::uniq (rida (x, xs));;