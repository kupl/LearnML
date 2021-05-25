let rec filter pred lst = 
match lst with | []->[] | hd::tl ->
(if pred hd == true then [hd] else [])@(filter pred tl);;
