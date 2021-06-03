let rec find
= fun item l -> 
  match l with
    | [] -> false
    | h::t -> h = item || find item t;;
    
let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec uniq' acc lst = 
  match lst with
    | [] -> acc
    | hd::tl -> if find hd tl then uniq' acc tl else uniq' (acc@[hd]) tl
    in uniq' [] lst;;
              

    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2  ->
    match l1 with
    | [] -> l2
    | hd::tl -> if find hd l2 then app tl l2
              else app tl (l2@[hd]);;


uniq [5;6;5;4];;
