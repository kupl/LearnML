let rec uniq (lst : 'a list) : 'b list =
  let rec mult_occurrence num (cntr : int) (lst : 'a list) : bool =
    match lst with
    | [] -> if cntr <= 1 then false else true
    | hd :: tl ->
        if num = hd then mult_occurrence num (cntr + 1) tl
        else mult_occurrence num cntr tl
  in

  let rec inner_uniq (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | hd :: tl ->
        if mult_occurrence hd 0 l then inner_uniq tl else hd :: inner_uniq tl
  in

  match lst with
  | [] -> []
  | __s3 :: __s4 -> __s3 :: uniq (List.filter (fun __s5 -> __s5 != __s3) __s4)


let (_ : int list) = uniq [ 5; 6; 5; 4; 8; 8; 8; 10; 10; 11 ]
