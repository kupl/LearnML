     let rec drop l n =
       match n with
         |0->l
           |_->let leng=List.length l in if leng<=n then [] else drop (List.tl l) (n-1);;
