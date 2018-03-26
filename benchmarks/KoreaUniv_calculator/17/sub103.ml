  (* problem 5*)
  type exp = X
           | INT of int
           | ADD of exp * exp           
           | SUB of exp * exp
           | MUL of exp * exp
           | DIV of exp * exp
           | SIGMA of exp * exp * exp

     let rec calculator : exp -> int
       = fun e -> (* TODO *)
        let rec etoi (exp,n)=
          match exp with
      | X->etoi(n,n)
      | INT n->n
      | ADD(f1,f2)->etoi(f1,n)+etoi(f2,n)
      | SUB(f1,f2)->etoi(f1,n)-etoi(f2,n)
      | MUL(f1,f2)->etoi(f1,n)*etoi(f2,n)
      | DIV(f1,f2) ->if etoi(f2,n)=0 then raise(Faliure "0 division") else etoi(f1,n)/etoi(f2,n)
      in
      match e with
      | INT i->i
      | ADD(f1,f2)->etoi(ADD(f1,f2),INT 0)
      | SUB(f1,f2)->etoi(SUB(f1,f2),INT 0)
      | MUL(f1,f2)->etoi(MUL(f1,f2),INT 0)
      | DIV(f1,f2)-> etoi(DIV(f1,f2),INT 0)
      | SIGMA (i1, i2, f1)->if etoi(SUB(i1,i2),INT 0)=0 then etoi(f1,i2) else     etoi(f1,i1)+calculator(SIGMA(ADD(INT 1,i1),i2,f1))
