let rec iter (n,(func : int -> int)) = if n = 0 then func
					else iter((n-1),func)
