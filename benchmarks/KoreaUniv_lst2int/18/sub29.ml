let lst2int : int list -> int
= fun lst -> 
  let rec savelstint : int list -> int -> int
  =fun slst sint -> 
    match slst with
      | [] -> sint
      | hd::tl -> 
        let exponent = truncate (log10 (float hd)) + 1 in
        let rec makemul : int -> int 
        = fun x ->
          if x <= 0 then 1
          else 10 * makemul (x - 1) in
        savelstint tl (sint * (makemul exponent) + hd) in
  savelstint lst 0;;

