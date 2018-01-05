let rec dured list =
	match list with 
	| [] -> []
	| [hd] -> [hd]
	| hd1 :: hd2 :: tl -> if hd1 = hd2 then dured (hd2::tl)
						  else hd1 :: dured (hd2::tl);;

let rec srt list=
let srtd = 
	match list with 
	|hd1::hd2::tl-> if hd1<hd2 then hd2::srt(hd1::tl)
					else hd1::srt(hd2::tl)
	|tl->tl in if list=srtd then list
			 	else srt srtd
;;

let merge ((lista:int list),(listb:int list)):int list= 
	match lista with 
	| []-> dured (srt listb)
	| hd::tl-> dured (srt(lista@listb))
;;

