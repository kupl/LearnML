let rec isin l1 a =
  match l1 with [] -> false | hd :: tl -> if a = hd then true else isin tl a


let rec uniq : 'a list -> 'a list =
 fun lst ->
  match lst with
  | [] -> []
  | hd :: tl -> if isin tl hd then uniq tl else hd :: uniq tl


let _ = uniq [ 5; 6; 5; 4 ]
