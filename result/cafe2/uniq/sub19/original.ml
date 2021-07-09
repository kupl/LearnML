let rec dupl a (lst : 'a list) : bool =
  match lst with [] -> false | hd :: tl -> if a = hd then true else dupl a tl


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> if dupl hd tl then uniq tl else hd :: uniq tl
