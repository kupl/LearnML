let rec remove_first a l =
  match l with 
    | [] -> []
    | hd::tl -> if a = hd then tl else [hd] @ remove_first a tl;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l2 with
    | [] -> l1
    | hd::tl -> if l2 = [] then l2 @ l1 
                else [hd] @ app (remove_first hd l1) tl;; 

app [1;3;5;2] [2;3;4;7];;    
