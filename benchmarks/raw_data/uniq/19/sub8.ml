let rec uniq lst =
  let rec uniq_lookup x y = 
    match x with
    | [] -> []
    | hd :: tl -> if y = hd then uniq_lookup tl y else hd::(uniq_lookup tl y) in
   match lst with
  | [] -> []
  | hd::tl -> hd::(uniq_lookup (uniq tl) hd);;
  uniq [5;6;5;4;3;2;2;1];;
