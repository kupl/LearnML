let rec uniq : 'a list -> 'a list =
 fun lst ->
  match lst with
  | [] -> []
  | [ a ] -> [ a ]
  | hd :: nx :: tl -> if hd = nx then uniq tl else hd :: uniq tl
