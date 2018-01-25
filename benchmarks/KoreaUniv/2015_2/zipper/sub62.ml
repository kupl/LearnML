let rec zipper ((a : int list),(b : int list)) = match a with
			| [] -> if b = [] then []
					else zipper(b,a)
			| hd::tl -> hd::(zipper(b,tl))
			