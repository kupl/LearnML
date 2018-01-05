let rec zipperN (li:int list list) =
  if (li = []) then []
  else let a = (List.hd li) in
    if (a = []) then (zipperN (List.tl li))
	else (List.hd a)::(zipperN ((List.tl li)@[(List.tl a)]))
