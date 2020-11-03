let rec insert a l = 
  match l with
    | [] -> [a]
    | hd::tl -> if a > hd then hd::(insert a tl)
                else if a = hd then l
                else a::l;;

let rec sort l1 =
  match l1 with
    | [] -> []
    | hd::tl -> insert hd (sort tl);;

let rec append l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> hd::(append tl l2);;

let app : 'a list -> 'a list -> 'a list
= fun l1 l2 ->
   match l1 with
    | [] -> sort l2
    | hd::tl -> sort (append l1 l2);;
    
app [3;4;5;6;7] [1;2;3;4;8];;    
(* TODO *)