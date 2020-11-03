let rec remove list relem count =
  match list with
  |[] -> []
  |hd::tl ->
  if (count=0 && hd=relem) then hd::(remove tl relem (count+1))
  else if (count<>0 && hd<>relem) then hd::(remove tl relem (count))
  else if (count<>0 && hd=relem) then remove tl relem (count+1)
  else hd::(remove tl relem (count));;

let rec length l =
match l with
|[]->0
|hd::tl -> 1+(length tl);;

let rec nth l n =
match l with
|[]->raise (Failure "list is too short")
|hd::tl-> if n=0 then hd else nth tl (n-1);;

let rec pftremove staylist chlist ncount nlimit =
  if ncount=(nlimit-1) then chlist
  else pftremove staylist (remove chlist (nth staylist ncount) 0) (ncount+1) (length staylist);;
  
  
let uniq : 'a list -> 'a list
= fun lst -> pftremove lst lst 0 (length lst);;

uniq [5;6;5;4];;
uniq [5;6;4;3;7;6;5;5;8;7;6;3;6];;
