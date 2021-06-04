let rec dupl a lst =
  match lst with [] -> false | hd :: tl -> if a = hd then true else dupl a tl


let rec uniq : 'a list -> 'a list =
 fun lst ->
  match lst with
  | [] -> []
  | hd :: tl -> if dupl hd tl then uniq tl else hd :: uniq tl
