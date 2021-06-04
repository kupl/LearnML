let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | [] -> []
  | [ a ] -> [ a ]
  | hd :: nx :: tl -> if hd = nx then uniq tl else hd :: uniq tl
