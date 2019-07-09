let rec reverse l = match l with
  | [] -> []
  | hd::tl -> (reverse tl)@[hd];;

let rec realLst2int 
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> hd + 10*realLst2int tl;;

let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> realLst2int(reverse lst);;
