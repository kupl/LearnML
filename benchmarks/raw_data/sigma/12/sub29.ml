exception NOMOVE of string

let rec sigma(init, uppr, func) =
	if (init < uppr) then (func init) + sigma(init+1, uppr, func)
	else if (init == uppr) then func init
	else 0
  
