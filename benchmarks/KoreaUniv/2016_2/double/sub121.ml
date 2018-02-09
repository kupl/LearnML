let rec double f a = 
	let temp = f a in f temp
