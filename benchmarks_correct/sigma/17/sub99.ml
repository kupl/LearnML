let rec sigma fnt a b =
  if (a > b) then
    0
  else
    fnt(a) + sigma fnt (a+1) b