let rec dup hd lst =
  match lst with
    [] -> false
    | h::t -> if h = hd then true else dup hd t;;

let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> (* TODO *)
    match l1 with
      [] -> l2
      | h::t -> if dup h l2 then app t l2 else app t (l2@[h]);;
      
app [4; 5; 6; 7] [1; 2; 3; 4];;
app [4; 4; 4; 7] [1; 4; 4; 4];;