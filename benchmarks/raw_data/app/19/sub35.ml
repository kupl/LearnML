let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  let rec insert a lst =
  match lst with
    | [] -> [a]
    | hd::tl -> if a < hd then a::hd::tl else if a = hd then insert a tl else hd::(insert a tl)
  in
  let rec asc lst = 
    match lst with
      | [] -> []
      | hd::tl -> insert hd (asc tl)
  in
  match asc l2 with
    | [] -> l1
    | hd::tl -> app (insert hd (asc l1)) tl;;