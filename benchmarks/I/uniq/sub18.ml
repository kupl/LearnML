let rec uniq : 'a list -> 'a list
= fun lst ->
  let rec is_member n lst' = 
    match lst' with 
    | [] -> false
    | hd::tl ->
      begin 
        if hd=n then true
        else is_member n tl
      end
      in 
  let rec loop lbuf =
    match lbuf with 
    | [] -> []
    | hd::tl ->
      begin
        let rbuf = loop tl
        in
          if is_member hd rbuf then rbuf
          else hd::rbuf
      end
      in
      loop lst;;

uniq [5;6;5;4;5];;