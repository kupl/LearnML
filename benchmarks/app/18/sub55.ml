(*let rec app : 'a list -> 'a list -> 'a list*)
(*= fun l1 l2 -> [] (* TODO *);;*)

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  let rec is_in_list num lst =
    match lst with
    | [] -> false
    | hd::tl -> if hd = num then true
      else is_in_list num tl in
        let rec app_inner lst1 lst2 = 
          match lst1 with
          | [] -> lst2
          | hd::tl -> if (is_in_list hd lst2) then (app_inner tl lst2)
                      else hd::(app_inner tl lst2)
              in app_inner l2 l1;;
              

app [4;5;6;7] [1;2;3;4;7;7;7];;