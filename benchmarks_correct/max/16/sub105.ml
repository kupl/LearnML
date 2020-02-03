(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst -> match lst with |[]->0|hd::[]->hd |hd::tl -> if hd>max tl then hd else max tl;;
 