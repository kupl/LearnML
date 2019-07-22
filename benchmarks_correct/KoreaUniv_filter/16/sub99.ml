let rec map f l =
	match l with
	|[] ->[]
	|hd::tl -> (if (f hd)=true then [hd] else [])@(map f tl)

let rec filter pred lst = map pred lst