let rec iter ((int_i : int), (f : 'a->'a)) : 'a->'a =
	if int_i<=0 then (fun x->x)
	else fun x -> f (iter(int_i-1, f)x)