let rec a 
= fun l1 l2 -> match l2 with
  | [] -> l1
  | hd::tl -> a (hd::l1) tl;; 

let fastrev : 'a list -> 'a list
= fun lst -> a [] lst;;

let rec f
= fun x lst -> match lst with
  |[] -> []
  |hd::tl -> if x = hd then f x tl else hd::(f x tl);; 

let rec b
= fun lst -> match lst with
  | [] -> []
  | hd::tl ->hd::b (f hd tl);;
  
let rec ap
= fun l1 l2 -> match l2 with
  |[] -> l1
  |hd::tl -> ap (hd::l1) tl;;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *) 
match l2 with
 |[] -> []
 |hd::tl -> let c = fastrev l2 in
   let d = ap l1 c in
     b d;;
     
app [1;2;3;4] [5;1;3;8];;

