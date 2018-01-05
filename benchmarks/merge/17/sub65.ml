let rec merge ((list1:int list),(list2:int list)):int list = 
 match (list1,list2) with
 | ([],_) -> list2
 | (_,[]) -> list1
 | (hd1::tl1,hd2::tl2) -> 
 			if(hd1>hd2) 
			 then (hd1::(merge(tl1,list2)))
 		 	 else (hd2::(merge(list1,tl2)))
			



(*test
let test1 = 4::2::1::[]
let test2 = 5::3::2::1::[]

let rec print_list = function
[]->print_newline()
|e::l -> print_int e ; print_string " "; print_list l


let _ = print_list(merge (test1,test2))
*)


