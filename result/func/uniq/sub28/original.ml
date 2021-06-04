let rec isin (l1 : 'a list) a : bool =
  match l1 with [] -> false | hd :: tl -> if a = hd then true else isin tl a


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> if isin tl hd then uniq tl else hd :: uniq tl


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
