
let rec f pred lst =
	match lst with
	|[]->[]
	|hd :: tl -> if pred hd then hd :: (f pred tl) else f pred tl;;