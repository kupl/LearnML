let rec lst2int : int list -> int
= fun lst ->
  match lst with
  | [] -> raise (Failure "empty list")
  | hd::[] -> hd
  | hd::tl -> int_of_string ((string_of_int hd)^(string_of_int (lst2int tl)));;