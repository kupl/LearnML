let rec comb lst a = 
  match lst with
    [] -> [a]
    |hd::tl -> if hd = a then lst else hd::(comb tl a);;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
  match l1 with
    [] -> l2
    |hd :: tl -> app tl (comb l2 hd);;
    
let rec uniq : 'a list -> 'a list
= fun lst ->  (* TODO *)
  app lst [];;
  
uniq [5;6;5;4] = [5;6;4];;
