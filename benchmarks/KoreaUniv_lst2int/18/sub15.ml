let rec digit l =
match l with
|[] -> 0
|_::[] -> 1 
|_::t -> 10 * digit t;;


let rec lst2int : int list -> int
= fun lst -> (*TODO*)
  match lst with 
    [] -> 0
    |hd::tl ->hd*digit lst + lst2int tl;;
