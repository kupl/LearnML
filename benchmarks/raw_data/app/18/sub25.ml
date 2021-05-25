let rec find
= fun item lst -> 
  match lst with
    | [] -> false
    | h::t -> h = item || find item t;;
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2  ->
    match l1 with
    | [] -> l2
    | hd::tl -> if find hd l2 then app tl l2
              else app tl (l2@[hd]);;


    
    find 4 [4;5;6;7];;
    

              
app [4;5;6;7] [1;2;3;4] ;;
