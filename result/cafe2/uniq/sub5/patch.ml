let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | __s3 :: __s4 -> __s3 :: uniq (List.filter (fun __s5 -> __s5 != __s3) __s4)
  | a :: b :: t -> if a = b then uniq t else a :: uniq t
  | smaller -> smaller


let (_ : int list) = uniq [ 1; 2; 3; 3 ]
