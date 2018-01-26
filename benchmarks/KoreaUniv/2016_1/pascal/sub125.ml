let rec pascal (x , y)  =
  if snd (x , y) == 0 then 1
	else if snd (x , y) == fst (x , y) then 1 
	else pascal (x - 1 , y - 1) + pascal (x - 1 , y);;
