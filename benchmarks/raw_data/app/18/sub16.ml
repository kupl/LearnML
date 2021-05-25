let rec chk = fun lst a -> match lst with
  [] -> true
  |hd::tl -> if (hd = a) then false else chk tl a;; 

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> match l2 with
  [] -> l1
  |hd::tl -> if (chk l1 hd) then hd :: app l1 tl else app l1 tl;;