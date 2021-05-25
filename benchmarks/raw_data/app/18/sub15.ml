let rec comb lst a = 
  match lst with
    [] -> [a]
    |hd::tl -> if hd = a then lst else hd::(comb tl a);;

let rec append : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
  match l1 with
    [] -> l2
    |hd :: tl -> append tl (comb l2 hd);;



let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  append l1 (append l2 []);;
    
    
  app [1;1][2;2];;