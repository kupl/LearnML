let rec dured lst =
	match lst with 
	| [] -> []
	| [hd] -> [hd]
	| hd1 :: hd2 :: tl -> if hd1 = hd2 then dured (hd2::tl)
						  else hd1 :: dured (hd2::tl);;

let rec srt lst=
let srtd = 
	match lst with 
	|hd1::hd2::tl-> if hd1<hd2 then hd2::srt(hd1::tl)
					else hd1::srt(hd2::tl)
	|tl->tl in if lst=srtd then lst
			 	else srt srtd
;;

let merge ((lista:int list),(listb:int list)):int list= 
	match lista with 
	| []-> dured (srt listb)
	| hd::tl-> dured (srt(lista@listb))
;;

