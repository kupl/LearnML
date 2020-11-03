let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1 with
    | [] -> l2

    | hd::tl -> 
      
      let rec search key lst = 
        match lst with
          | [] -> false
        
          | hd::tl -> if key = hd then true else search key tl
        
      in if (search hd l2) = true then app tl l2
         
         else let new_l2 = l2@[hd] 
              in app tl new_l2;;

(*output*)
app [4;5;6;7] [1;2;3;4];;
app [4;5;6;7] [4;5;6;7;9];;