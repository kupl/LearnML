(*2006-11720 Kim Eunsol*)
let rec sigma(a, b, f) = if a<b then f(a) + sigma(a+1, b, f)
						else f(b)
