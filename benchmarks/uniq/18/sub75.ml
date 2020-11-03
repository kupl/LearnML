let rec uniq n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: uniq (n-1) t;;
   uniq 2 ["5";"6";"5";"4"];;