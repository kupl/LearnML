let rec search k = function
| [] -> false
| hd::tl -> hd = k || search k tl
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
| [] -> l2
| hd::tl -> if search hd l2 then app tl l2 
   else app tl (hd::l2);;

