let rec uniq (lst : 'b list) : 'a list =
  match lst with [] -> [] | hd :: tl -> hd :: uniq (search hd tl)


and search s (l : 'b list) : 'a list =
  match l with
  | [] -> []
  | hd :: tl -> if s = hd then search s tl else hd :: search s tl


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
