let rec f func a b =
	let rec sum func n =
	if n>1 then func n+ sum func (n-1)
	else if n=1 then 1
	else 0
in 
sum func b - sum func (a-1)

