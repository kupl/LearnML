type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with

  | INT i->i
  | ADD(a,b)->calculator a+calculator b
  | SUB(a,b)-> calculator a-calculator b 
  | MUL(a,b)->calculator a*calculator b
  | DIV(a,b)->(calculator a) / (calculator b)
  | SIGMA (a,b,c)-> let rec sigma1 l q= match q with
    |X->l
    |INT i->i
    |ADD(x,y)->begin match x with 
      |X->if y=X then l+l else l+(sigma1 l y)
      |_-> match y with 
        |X->(sigma1 l x) + l
        |_->(sigma1 l x)+(sigma1 l y)
        end
    | SUB (x,y)-> begin match x with 
      |X->if y=X then l-l else l-(sigma1 l y)
      |_-> match y with 
        |X->(sigma1 l x)-l
        |_->(sigma1 l x)- (sigma1 l y)
        end
    | MUL(x,y)->begin match x with 
      |X->if y=X then l* l else l*(sigma1 l y)
      |_-> match y with 
        |X->(sigma1 l x) * l
        |_->(sigma1 l x )* (sigma1 l y)
        end
    | DIV(x,y)->begin match x with 
      |X->if y=X then 1 else l/(sigma1 l y)
      |_-> match y with 
        |X->(sigma1 l x )/ l
        |_->(sigma1 l x)/(sigma1 l y)
        end
        
    in let k=calculator b in let u =calculator a in 
    let rec sigma2 o p c= if o=p then sigma1 p c else sigma1 p c+ sigma2 o (p-1) c in sigma2 u k c;;
  