let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | [] -> []
  | [ a ] -> [ a ]
  | __s3 :: __s4 -> __s3 :: uniq (List.filter (fun __s5 -> __s5 != __s3) __s4)
  | hd :: nx :: tl -> if hd = nx then uniq tl else hd :: uniq tl
