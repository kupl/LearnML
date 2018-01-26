(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let diff : aexp * string -> aexp
  = fun (exp, var) ->    let rec f e v =match e with
		| Const a -> Const 0
  		| Var a -> if a=var then Const 1 else Const 0
  		| Power(x,y)-> if x=var then Times [Const y; Power (x, (y-1))] else Const 0
		| Sum lst->  begin match lst with
			|[]-> raise (Failure (" No elements in the list! "))
			|hd::tl-> if tl==[] then f hd var else Sum ([f hd var]@[f (Sum tl) var])
				end	
		| Times lst ->  match lst with
			|[]-> raise (Failure (" No elements in the list! "))
			|hd::tl-> if tl==[] then f hd var else  match hd with
					|Const a -> Times ([hd]@[f (Times tl) var])
					|Var a -> if hd=(Var var) then Sum ([f (Times tl) var]@([hd]@[f (Times tl) var])) else Times ([hd]@[f (Times tl) var])
					|Power(x,y)-> if  (Var x)=(Var var) then Sum ([Times ([f hd var]@tl)]@[Times ([hd]@[f (Times tl) var])]) else Times ([hd]@[f (Times tl) var])		
					|Sum ist -> Sum( [Times ([f hd var]@tl)]@[Times ([hd]@[f (Times tl) var])])
					|Times ist -> Sum( [Times ([f hd var]@tl)]@[Times ([hd]@[f (Times tl) var])])
								  
										  
	in f exp var

end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

  let balanced : mobile -> bool
  = fun mob ->  let rec f mob = match mob with
		| (SimpleBranch (a,b) , SimpleBranch (c,d)) -> if (a*b)==(c*d) then true else false
		| (SimpleBranch (a,b) , CompoundBranch (c,d)) ->  let rec g d = match d with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if ((a*b)=(c*(g d)))&&(f d) then true else false
		| (CompoundBranch (a,b) , SimpleBranch (c,d)) ->  let rec g b = match b with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if ((c*d)=(a*(g b)))&&(f b) then true else false
		| (CompoundBranch (a,b) , CompoundBranch (c,d)) ->   let rec g b = match b with
						| (SimpleBranch (x,y) , SimpleBranch (z,t)) -> y+t
						| (SimpleBranch (x,y) , CompoundBranch (z,t)) -> if (f t) then y+(g t) else 100000000
						| (CompoundBranch (x,y) , SimpleBranch (z,t)) -> if (f y) then t+(g y) else 100000000
						| (CompoundBranch (x,y) , CompoundBranch (z,t))-> if (x*(g y))==(z*(g t)) then (g y)+(g t) else 10000000
					in if (a*(g b))=(c*(g d)) then true else false
	in f mob
end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
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
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let check : exp -> bool
  = fun exp -> let rec e exp = match exp with
		| V v -> false
		| P (p,l) -> let rec f p l = begin match l with 
				|V a -> if a=p then true else false
				|P (b,c) -> if (f p c)||(f b c) then true else false
				|C (d,e) -> if (f p e)&&(f p d) then true else false 	
					end
					in f p l
		| C (n,m) ->  match n,m with
			|(V x),(V y) -> false
			|(P (x,y)),(P (z,t)) -> if ((e (P(x,y)))||(e (P(z,y))))&&((e (P(x,t)))||(e (P(z,t)))) then true else false
			|(V x),(P (y,z)) -> if ((e (P(y,n)))||(e (C(n,z))))&&((e (P(y,z)))||(e (C(n,z)))) then true else false
			|(P (y,z)),(V x) -> if ((e (P(y,m)))||(e (C(m,z))))&&((e (P(y,z)))||(e (C(m,z)))) then true else false
			|(V x),(C (y,z)) -> if ((e (C(n,y)))||(e (C(n,z))))&&((e (C(y,z)))||(e (C(y,n))))&&((e (C(z,n)))||(e (C(z,y)))) then true else false
			|(C (y,z)),(V x) -> if ((e (C(m,y)))||(e (C(m,z))))&&((e (C(y,z)))||(e (C(y,m))))&&((e (C(z,m)))||(e (C(z,y)))) then true else false
			|(P (x,y)),(C (z,t)) ->if ((e (P(x,y)))||(e (C(y,z)))||(e (C(y,t))))&&((e (P(x,z)))||(e (C(z,t)))||(e (C(z,y))))&&((e (P(x,t)))||(e (C(t,y)))||(e (C(t,z)))) then true else false
			|(C (z,t)),(P (x,y)) ->if ((e (P(x,y)))||(e (C(y,z)))||(e (C(y,t))))&&((e (P(x,z)))||(e (C(z,t)))||(e (C(z,y))))&&((e (P(x,t)))||(e (C(t,y)))||(e (C(t,z)))) then true else false
			|(C (x,y)),(C (z,t)) -> if ((e (C(x,y)))||(e (C(x,z)))||(e (C(x,t))))&&((e (C(y,z)))||(e (C(y,t)))||(e (C(y,x))))&&((e (C(z,t)))||(e (C(z,x)))||(e (C(z,y))))&&((e (C(t,x)))||(e (C(t,y)))||(e (C(t,z)))) then true else false	
		in e exp

end

