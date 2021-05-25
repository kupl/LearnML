let rec dupl
= fun a lst -> match lst with
    | [] -> false
    | hd::tl -> if a = hd then true else (dupl a tl)

let get
= fun (lst1, lst2) ->
  let rec del
  = fun lst1 -> match lst1 with
      | [] -> []
      | hd::tl -> if dupl hd lst2 then del tl else hd::(del tl)
  in lst2@(del lst1)

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> get (l1, l2);;
