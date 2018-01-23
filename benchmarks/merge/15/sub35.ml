(*merge list a, b*)
let rec my_sort s = 
	let rec _usort = function
		|x::x2::xt->
			if(x>x2) then x:: _usort(x2::xt)
      else x2 :: _usort(x::xt)
		|s->s
	in
	let t = _usort s in
		if t = s then t
		else my_sort t
		

let merge a b = my_sort (a@b)
