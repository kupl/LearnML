let rec exist lst a = match lst with
                      | [] -> false
                      | hd::tl -> hd = a || (exist tl a);;

let rec newlst l1 l2 = match l1 with
                       | [] -> []
                       | hd::tl -> if exist l2 hd then newlst tl l2 else hd::newlst tl l2;;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> l2 @ newlst l1 l2;; 