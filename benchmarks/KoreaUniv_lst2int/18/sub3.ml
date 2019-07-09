let rec length : int list -> int = fun lst ->
  match lst with
    | [] -> 1
    | hd::tl -> 10 * (length tl);;
    
let lst2int : int list -> int
= fun lst -> (*TODO*)
  let rec tmp lst =
    match lst with
      | [] -> 0
      | hd::tl -> hd * (length tl) + (tmp tl) in tmp lst;;
