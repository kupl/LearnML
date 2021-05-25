let rec sigma ((startnum : int), (endnum : int), (fnt : (int -> int))) : int =
  if (startnum > endnum) then
    0
  else
    fnt(startnum) + sigma(startnum + 1, endnum, fnt)