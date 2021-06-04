let rec check_dup a (l : 'a list) : bool =
  match l with
  | [] -> true
  | hd :: tl -> if a = hd then false else check_dup a tl


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> if check_dup hd tl = true then hd :: uniq tl else uniq tl
