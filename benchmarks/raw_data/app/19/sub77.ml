let rec duplicate x lst =
    match lst with
      | [] -> false
      | hd::tl -> if x = hd then true else duplicate x tl 

exception Not_found;;

let rec remove a lst=
  match lst with
    | []->[]
    | hd :: tl -> if hd = a then remove hd tl else hd:: remove a tl;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> 
  match lst with 
    | [] -> []
    | hd::tl -> if duplicate hd tl then hd::remove hd (uniq tl) else hd::(uniq tl);;
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l1,l2 with
  | [],_ -> l2
  | _,[] -> l1
  | hd1::tl1, hd2::tl2 ->if hd1 = hd2 then uniq(hd1:: app tl1 tl2)
                         else if hd1 < hd2 then uniq(hd1 :: app tl1 l2)
                           else uniq(hd2 :: app l1 tl2);;

    
 
    
    app [4;5;6;7] [1;2;3;4];;
    app [4;2;6;9] [1;2;3;6];;