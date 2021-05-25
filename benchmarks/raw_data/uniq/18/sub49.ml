let rec judge
= fun x l ->
  match l with 
  | [] -> false
  | hd::tl -> hd = x || judge x tl;;
  
let rec uniq : 'a list -> 'a list
= fun lst -> 
let rec l1 lst l2 =
  match lst with
  | [] -> l2
  | hd::tl -> if judge hd l2 then l1 tl l2 
              else l1 tl (l2@hd::[]) in l1 lst [];;
  
  uniq[5;6;5;4];;