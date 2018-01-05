type crazy2=NIL
           |ZERO of crazy2
           |ONE of crazy2
           |MONE of crazy2

let rec incr:crazy2->crazy2=fun(a)->
  match a with
  |NIL->ONE NIL
  |ZERO c->ONE c
  |ONE c->ZERO (incr c)
  |MONE c->ZERO c

let rec decr:crazy2->crazy2=fun(a)->
  match a with
  |NIL->MONE NIL
  |ZERO c->MONE c
  |ONE c->ZERO c
  |MONE c->ZERO (decr c)

let rec crazy2add:crazy2*crazy2->crazy2=fun(a,b)->
  match (a,b) with
  |(NIL,d)->b
  |(c,NIL)->a
  |(ZERO c,ZERO d)->ZERO(crazy2add(c,d))
  |(ZERO c,ONE d)->ONE(crazy2add(c,d))
  |(ZERO c,MONE d)->MONE(crazy2add(c,d))
  |(ONE c,ZERO d)->ONE(crazy2add(c,d))
  |(ONE c,ONE d)->ZERO(crazy2add(c,incr d))
  |(ONE c,MONE d)->ZERO(crazy2add(c,d))
  |(MONE c,ZERO d)->MONE(crazy2add(c,d))
  |(MONE c,ONE d)->ZERO(crazy2add(c,d))
  |(MONE c,MONE d)->ZERO(crazy2add(c,decr d))

