let rec digit = fun lst -> match lst with
  [] -> 1
  |hd::tl -> 10 * digit(tl);;

let rec lst2int : int list -> int
= fun lst -> match lst with
  [] -> 0
  |hd::tl -> (hd * digit(tl)) + lst2int tl;;