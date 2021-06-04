let rec uniq (lst : 'b list) : 'a list =
  match lst with
  | a :: b :: t -> if a = b then uniq t else a :: uniq t
  | smaller -> smaller


let (_ : int list) = uniq [ 1; 2; 3; 3 ]
