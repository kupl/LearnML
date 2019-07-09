let rec len lst = match lst with
  | [] -> 0
  | hd :: tl -> 1 + len tl;;

let rec pow n ex = match ex with
  | 0 -> 1
  | _ -> n * pow n (ex-1);;

let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd :: tl -> let n = len tl in hd * (pow 10 n) + (lst2int tl);;