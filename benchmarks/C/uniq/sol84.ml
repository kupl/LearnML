let rec exist lst a = match lst with
                  | [] -> false
                  | hd::tl -> hd = a || exist tl a;;

let rec newlst l1 l2 = match l1 with
                      | [] -> []
                      | hd::tl -> if exist l2 hd then newlst tl l2 else hd::newlst tl (hd::l2);;

let uniq : 'a list -> 'a list
= fun lst -> newlst lst [];;