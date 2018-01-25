  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec ptree : var * exp -> bool
  =fun (e1,e2) -> match e2 with
  | P (x,y) -> 
  (match y with
	| P (a,b) -> ptree(a,b) || ptree(x,b) || ptree(e1,b)
	| C (a,b) -> (ptree(x,a)||ptree(x,b)) && (ptree(e1,a)||ptree(e1,b))
	| V a -> ptree(x,y) || ptree(e1,y)
	)
  | C (x,y) -> ptree(e1,x) && ptree(e1,y)
  | V x -> e1=x
  
  let rec ctree : exp * exp -> bool
  =fun (e1,e2) -> match e1,e2 with
  | P (x1,y1), P (x2,y2) -> ptree(x1,y1) && ptree(x2,y2)
  | C (x1,y1), P (x2,y2) -> ctree(x1,y1) && ptree(x2,y2)
  | P (x1,y1), C (x2,y2) -> ptree(x1,y1) && ctree(x2,y2)
  | C (x1,y1), C (x2,y2) -> ctree(x1,y1) && ctree(x2,y2)
  | V x, _ -> false
  | _, V y -> false
  
  let check : exp -> bool
  =fun e -> match e with
  | P (x,y) -> ptree (x,y)
  | C (x,y) -> ctree (x,y)
  | V x -> true
