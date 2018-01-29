
type exp = V of var
				 | P of var * exp
				 | C of exp * exp
and var = string


let rec fv 
	= fun (va,la) -> match la with
	|[] -> false
	| hd::tl -> if (va = hd) then true else fv(va,tl)

let rec findv
	= fun(a,la) -> match a with
	|V((b:var)) -> fv(b,la)
	|P((va:var),a1)-> findv(a1,(la @ [va]))
	|C(a1,a2) -> if (findv(a1,la)&&findv(a2,la)) = true then true else false;;

let check : exp -> bool
= fun a -> findv(a,[]);;
