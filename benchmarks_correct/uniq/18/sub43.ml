let rec cz = fun x lst ->
  match lst with
    |[] ->false
    |hd::tl ->(hd==x) || (cz x tl);; 

let rec uniq : 'a list -> 'a list
= fun lst -> 
 let rec l lst le=
    match lst with
    |[]->le
    |hd::tl -> if cz hd le then l tl le
              else l tl(le@[hd]) in l lst [];;

uniq [5;6;5;4];;