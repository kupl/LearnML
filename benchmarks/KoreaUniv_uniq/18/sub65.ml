let rec uniq : 'a list -> 'a list
=	fun x->
	let rec drop_sub : 'a ->'a list -> 'a list =
    fun target lst ->
      match lst with
        |hd::tl -> if(hd=target) then drop_sub target tl else hd::(drop_sub target tl)
        |_ -> [] in
	match x with
	|hd::tl -> hd::(uniq (drop_sub hd tl))
	|_ -> [];;