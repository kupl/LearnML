let fst (x,_) = x;;
let snd (_,x) = x;;

let rec unzip_fst lst = match lst with
							| [] -> []
							| h::t -> ((fst h)::(unzip_fst t));;
let rec unzip_snd lst = match lst with
							| [] -> []
							| h::t -> ((snd h)::(unzip_snd t));;
let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> ((unzip_fst lst),(unzip_snd lst));;