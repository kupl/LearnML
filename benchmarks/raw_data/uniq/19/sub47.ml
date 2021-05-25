
let rec dupl a l =
  match l with
    |[] -> false
    |hd::tl -> if hd = a then true else dupl a tl;;
    
let rec uniq lst =
  match lst with
    |[] -> []
    |hd::tl -> 
      if dupl hd tl then uniq tl
      else hd :: uniq tl ;;
                
  uniq [5;6;5;4] ;;
  uniq [3;6;8;4;6;3;7;9;8];;
