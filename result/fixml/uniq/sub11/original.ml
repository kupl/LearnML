let rec uniq : 'a list -> 'a list =
 fun lst ->
  let rec mult_occurrence num cntr lst =
    match lst with
    | [] -> if cntr <= 1 then false else true
    | hd :: tl ->
        if num = hd then mult_occurrence num (cntr + 1) tl
        else mult_occurrence num cntr tl
  in

  let rec inner_uniq l =
    match l with
    | [] -> []
    | hd :: tl ->
        if mult_occurrence hd 0 l then inner_uniq tl else hd :: inner_uniq tl
  in
  inner_uniq lst


let _ = uniq [ 5; 6; 5; 4; 8; 8; 8; 10; 10; 11 ]
