(*let lst2int : int list -> int*)
(*= fun lst -> (*TODO*)*)

let rec lst2string : int list -> string
= fun lst ->
  match lst with
     [] -> ""
    | h::t ->  (string_of_int h)^(lst2string t)
;;

let rec lst2int : int list -> int
= fun lst ->
  int_of_string(lst2string lst)
;;