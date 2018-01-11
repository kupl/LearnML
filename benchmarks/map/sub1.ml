let rec map func l =
  match l with
  |[] -> []
  |hd::tl -> hd::tl;; 
