let rec remove (l : 'a list) relem (count : int) : 'a list =
  match l with
  | [] -> []
  | hd :: tl ->
      if count = 0 && hd = relem then hd :: remove tl relem (count + 1)
      else if count != 0 && hd != relem then hd :: remove tl relem count
      else if count != 0 && hd = relem then remove tl relem (count + 1)
      else hd :: remove tl relem count


let rec length (l : 'b list) : int =
  match l with [] -> 0 | hd :: tl -> 1 + length tl


let rec nth (l : 'c list) (n : int) =
  match l with
  | [] -> raise Failure "list is too short"
  | hd :: tl -> if n = 0 then hd else nth tl (n - 1)


let rec pftremove (staylist : 'c list) (chlist : 'c list) (ncount : int)
    (nlimit : int) : 'c list =
  if ncount = nlimit - 1 then chlist
  else
    pftremove staylist
      (remove chlist (nth staylist ncount) 0)
      (ncount + 1) (length staylist)


let uniq (lst : 'c list) : 'd list = pftremove lst lst 0 (length lst)
