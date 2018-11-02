(*problem 3*)

  type formula =
      True
      |False
      |Var of string
      |Neg of formula
      |And of formula *formula
      |Or of formula *formula
      |Imply of formula *formula
      |Iff of formula *formula

  let sat : formula -> bool
  =fun f ->
        true           
            
  
  ;;

