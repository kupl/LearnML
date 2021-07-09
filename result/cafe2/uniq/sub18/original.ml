let rec uniq (lst : 'a list) : 'b list =
  let rec is_member n (lst' : 'a list) : bool =
    match lst' with
    | [] -> false
    | hd :: tl -> if hd = n then true else is_member n tl
  in

  let rec loop (lbuf : 'a list) : 'a list =
    match lbuf with
    | [] -> []
    | hd :: tl ->
        let rbuf : 'a list = loop tl in
        if is_member hd rbuf then rbuf else hd :: rbuf
  in
  loop lst


let (_ : int list) = uniq [ 5; 6; 5; 4; 5 ]
