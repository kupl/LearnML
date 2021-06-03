let rec sigma f a b =
(
	if (b < a) then (0)
	else ( f b + sigma f a (b-1))

)
