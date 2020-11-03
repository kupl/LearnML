let rec uniq : 'a list -> 'a list
= fun lst -> match lst with 
  | [] -> []
  | hd::tl -> let rec drop a lst' = match lst' with
    | [] -> []
    | hd'::tl' -> if a = hd' then drop a tl' else hd'::(drop a tl')
      in hd::uniq(drop hd tl);;
    
uniq [5;6;5;4];;