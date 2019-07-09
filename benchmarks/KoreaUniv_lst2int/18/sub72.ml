let rec leng l =
  match l with
    | [] -> 0
    | hd::tl -> 1 + leng tl;;
    
let rec tenroot a = 
  if(a = 1) then 1
  else 10 * tenroot (a-1);;

let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> hd * tenroot (leng lst) + lst2int tl;;

lst2int [2;3;4;5;];;
