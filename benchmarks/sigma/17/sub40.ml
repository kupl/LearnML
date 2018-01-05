let rec sigma ((beg : int), (fin: int), (f: int -> int)) : int =
  if (beg > fin)
    then 0
    else (f beg) + (sigma (beg+1, fin, f))
