let lst2int : int list -> int
= fun lst -> let rec reverse : int list -> int list 
= fun l -> match l with
  [] -> []
  |h::t -> (reverse t)@[h] in
  let rec revlst2int : int list -> int
= fun l -> 
  match l with
  [] -> 0
  |h::t -> h + revlst2int(t) * 10 in
  revlst2int (reverse lst);;

lst2int [2;3;4;5];;