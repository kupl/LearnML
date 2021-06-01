let rec sigma func min max =
	if min > max then 0
	else if min = max then func min
	else func min + sigma func (min+1) max
