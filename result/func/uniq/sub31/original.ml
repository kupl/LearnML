let rec _contains (lst : 'a list) value : bool =
  match lst with
  | [] -> false
  | hd :: tl -> if hd = value then true else _contains tl value


let rec uniq (lst : 'c list) : 'b list =
  match lst with
  | [] -> []
  | hd :: tl -> if _contains tl hd then uniq tl else hd :: uniq tl
