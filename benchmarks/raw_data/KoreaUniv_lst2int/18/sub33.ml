let rec reverse n =
match n with
  [] -> []
| hd::tl -> (reverse tl) @ [hd]

let lst2int : int list -> int
= fun lst -> 
let rec sum n =
match n with
  [] -> 0
| hd::tl -> hd+ 10* (sum tl) in
          sum(reverse lst);;
 
 
lst2int [2;3;4;5];;
lst2int [2;3;4;5;6;7;9];;