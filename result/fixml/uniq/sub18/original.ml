let rec uniq : 'a list -> 'a list =
 fun lst ->
  let rec is_member n lst' =
    match lst' with
    | [] -> false
    | hd :: tl -> if hd = n then true else is_member n tl
  in

  let rec loop lbuf =
    match lbuf with
    | [] -> []
    | hd :: tl ->
        let rbuf = loop tl in
        if is_member hd rbuf then rbuf else hd :: rbuf
  in
  loop lst


let _ = uniq [ 5; 6; 5; 4; 5 ]
