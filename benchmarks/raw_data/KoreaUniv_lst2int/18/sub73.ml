let adder : int -> int list -> int list
= fun a l -> match l with
  | [] -> []
  | hd::tl -> (hd+a*10)::tl;;

let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | [a] -> a
  | hd::tl -> lst2int(adder hd tl);;
  