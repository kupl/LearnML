let rec is_not_equal lst a =
  match lst with 
    |[]-> true
    |h::t-> if h=a then false else (is_not_equal t a);;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
   match l1 with 
     |[]-> l2
     |h::t-> if (is_not_equal l2 h) then (app t (l2@[h])) else app t l2;; 
     

 app [4;5;6;7] [1;2;3;4] ;;