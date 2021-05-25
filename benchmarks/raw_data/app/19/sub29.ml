let rec forall : ('a -> bool) -> 'a list -> bool
= fun p lst ->
  match lst with
  |[] -> true
  |hd::tl -> p hd && forall p tl;;
    
let rec app : 'a list -> 'a list -> 'a list
= fun l1 l2 -> 
  match l2 with
  | [] -> l1
  | _ ->
    match l1 with
    | [] -> l2
    | hd1::tl1 -> if forall (fun x -> x<>hd1) l2 then app l1 (l2@[hd1])
    else app tl1 l2;;

app [4;5;5;3] [1;5;4;4];;
