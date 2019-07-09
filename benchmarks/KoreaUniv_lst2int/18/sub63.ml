let rec foldl
= fun f l s -> match l with
  [] -> s
  | h::t -> foldl f t (f h s);;

let rec lst2int : int list -> int
= fun lst -> int_of_string (foldl (fun x y -> y ^ (string_of_int x)) lst "");;