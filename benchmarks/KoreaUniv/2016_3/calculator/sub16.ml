
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp ->   let rec f exp = match exp with
		|X -> raise (Failure("Wrong Input!"))
		|INT a -> a
		|ADD (a,b) -> (f a)+(f b)
		|SUB (a,b) -> (f a)-(f b)
		|MUL (a,b) -> (f a)*(f b)
		|DIV (a,b) -> (f a)/(f b)
		|SIGMA (a,b,c) -> let rec h i r= if i=(f b) then fn i r else (fn i r)+(h (i+1) r)
				in h (f a) c


and fn : int -> exp -> int = fun x y -> let rec g x y= match y with
			|X -> x
			|INT q-> q
			|ADD (q,w) -> (g x q)+(g x w)
			|SUB (q,w) -> (g x q)-(g x w)
			|MUL (q,w) -> (g x q)*(g x w)
			|DIV (q,w) -> (g x q)/(g x w)
			|SIGMA (q,w,m)-> match q, w with
						|INT a,INT b-> let rec j k l = if k==b then g k l else (g k l)+(j (k+1) l)
								in j a m
						|_-> raise (Failure("Wrong Input!"))
 in g x y
in f exp