let rec lst2int : int list -> int
= fun lst -> match lst with
  | [] -> 0
  | hd::tl -> let rec listlength : int list -> int
    = fun lst -> match lst with
      | [] -> 0
      | hd::tl -> int_of_float (log10(float_of_int hd)) + 1 + listlength tl 
        in let rec mul n1 n2 = match n2 with
            | 0 -> 1
            | _ -> n1 * mul n1 (n2-1)
              in hd * (mul 10 (listlength tl)) + lst2int tl;;


lst2int [2;3;4;5];; 
