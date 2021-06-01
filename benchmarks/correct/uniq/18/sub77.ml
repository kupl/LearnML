let rec equalcheck
= fun n lst ->
  match lst with 
    |[] -> false
    |hd::tl -> (hd==n) || (equalcheck n tl);;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  let rec loop = fun lst tmp ->
    match lst with
      |[] -> tmp
      |hd::tl -> if equalcheck hd tmp then loop tl tmp else loop tl (tmp@hd::[]) in loop lst [];;
      
uniq [5;6;5;4];;