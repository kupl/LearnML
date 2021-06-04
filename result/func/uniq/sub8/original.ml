let rec thereis a (l : 'a list) : bool =
  match l with [] -> false | hd :: tl -> if a = hd then true else thereis a tl


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> if thereis hd tl then uniq tl else hd :: uniq tl


let (_ : int list) = uniq [ 5; 6; 5; 4 ]
