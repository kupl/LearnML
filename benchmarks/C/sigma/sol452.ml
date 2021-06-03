let rec sigma f beg fin =
  if (beg > fin)
    then 0
    else (f beg) + (sigma f (beg+1) fin)
