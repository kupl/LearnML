(*2006-11720 Kim Eunsol*)
let rec sigma f a b = if a<b then f(a) + sigma f (a+1) b
						else f(b)
