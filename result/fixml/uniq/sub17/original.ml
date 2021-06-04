let rec dup a l =
  match l with [] -> false | hd :: tl -> if a = hd then true else dup a tl


let rec uniq : 'a list -> 'a list =
 fun lst ->
  match lst with
  | [] -> []
  | hd :: tl -> (
      match tl with
      | [] -> [ hd ]
      | hd2 :: tl2 -> if dup hd tl then uniq tl else hd :: uniq tl )


let _ = uniq [ 1; 2; 3; 3; 4; 4 ]
