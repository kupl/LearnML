let rec __s4 __s5 (__s6 : 'a list) : 'a list =
  match __s6 with
  | [] -> []
  | __s11 :: __s12 ->
      if __s11 = __s5 then __s4 __s11 __s12 else __s11 :: __s4 __s5 __s12


let rec uniq (lst : 'b list) : 'c list =
  let rec is_member n (lst' : 'b list) : bool =
    match lst' with
    | [] -> false
    | hd :: tl -> if hd = n then true else is_member n tl
  in

  let rec loop (lbuf : 'b list) : 'b list =
    match lbuf with
    | [] -> []
    | hd :: tl ->
        let rbuf : 'b list = loop tl in
        if is_member hd rbuf then rbuf else hd :: rbuf
  in

  match lst with
  | [] -> []
  | __s9 :: __s10 ->
      if is_member __s9 __s10 then __s9 :: __s4 __s9 (uniq __s10)
      else __s9 :: uniq __s10


let (_ : int list) = uniq [ 5; 6; 5; 4; 5 ]
