exception Error of string

let read1 n =
	match n with
		1 -> "��"
		| 2 -> "��"
		| 3 -> "��"
		| 4 -> "��"
		| 5 -> "��"
		| 6 -> "��"
		| 7 -> "ĥ"
		| 8 -> "��"
		| 9 -> "��"
		| _ -> raise (Error "size error")
;;

let read2 n =
	match n with
		1 -> "��"
		| 2 -> "��"
		| 3 -> "õ"
		| _ -> raise (Error "size error")
;;

let rec read (num, a) =
	if ((num=0) && (a=0)) then ["��"]
	else if (num = 0) then []
	else if (num mod 10 = 0) then read (num/10, a+1)
	else if (a = 0) then List.append (read(num/10, a+1)) [read1 (num mod 10)]
	else if (num mod 10 = 1) then List.append (read (num/10, a+1)) [read2 (a)]
	else List.append (read (num/10, a+1)) [read1 (num mod 10); read2 (a)]	
;;

let vocalize s = 
	 (read( int_of_string(s)/10000, 0 )) :: (read( int_of_string(s) mod 10000, 0 )) :: []

;;