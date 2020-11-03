let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> let rec uniq : 'a list -> 'a list
  = fun lst -> match lst with 
    | [] -> []
    | hd::tl -> let rec drop a lst' = match lst' with
      | [] -> []
      | hd'::tl' -> if a = hd' then drop a tl' else hd'::(drop a tl')
        in hd::uniq(drop hd tl)
          in uniq (l2@l1);;
        (*let rec droplist l1' l2' = match l1' with
  | [] -> l2'
  | hd'::tl' -> let rec drop a lst = match lst with
    | [] -> []
    | hd::tl -> if a = hd then drop a tl else hd::(drop a tl)
      in droplist tl' (drop hd' l2')
        in l2@(droplist l2 l1);;
        *)
app [4;5;6;7] [1;2;3;4];;

