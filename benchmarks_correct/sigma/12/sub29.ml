exception NOMOVE of string

let rec sigma func init uppr =
	if (init < uppr) then (func init) + sigma func (init+1) uppr
	else if (init == uppr) then func init
	else 0
  
