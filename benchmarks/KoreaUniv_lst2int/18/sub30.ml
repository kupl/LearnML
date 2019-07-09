let rec mul10 : int -> int
= fun rep -> match rep with
  | 0 -> 1
  | _ -> 10 * (mul10 (rep - 1));;

let rec lstlen : 'a list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> 1 + (lstlen tl);;
  
let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> hd * (mul10 (lstlen tl)) + (lst2int tl);;
