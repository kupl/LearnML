type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
					|True -> true
					|False -> false
					|Neg a -> (match a with
										|True->false
										|False->true
										|_-> if eval a = true then false else true)
					|Or (a,b) -> (match (a,b) with
											|(True,True)->true
											|(True,False)->true
											|(False,True)->true
											|(False,False)->false
											|_->if (eval a = true || eval b = true) then true else false)
					|And (a,b) -> (match (a,b) with
											|(True,True)->true
											|(True,False)->false
											|(False,True)->false
											|(False,False)->false
										  |_->if (eval a = true && eval b = true) then true else false)

					|Imply (a,b) -> (match (a,b) with
												|(True,True)->true
												|(True,False)->false
												|(False,True)->true
												|(False,False)->true
												|_->if (eval a = true && eval b = false) then false else true)
					|Equiv (a,b) -> (match (a,b) with
													|(True,True)->true
													|(True,False)->false
													|(False,True)->false
													|(False,False)->true
													|_->if eval a = eval b then true else false);;


	

eval (Or (True,False));;

