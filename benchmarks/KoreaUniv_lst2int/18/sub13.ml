let rec lst2int : int list -> int
= fun lst ->
  let rec reverse l =
    match l with
      [] -> []
      | hd::tl-> (reverse tl)@[hd] in 

  let rlst = (reverse lst) in
  let rec lint = fun x ->
    match x with
     [] -> 0
      |hd::tl -> hd+10*(lint tl)in
  (lint rlst);;
   


