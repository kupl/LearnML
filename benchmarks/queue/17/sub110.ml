module type Queue=
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ:queue
    val enQ:queue*element->queue
    val deQ:queue->element*queue
  end

module IntListQ=
  struct
    type element=int list
    type queue=element list*element list
    exception EMPTY_Q
    let emptyQ:queue=([],[])
    let enQ:queue*element->queue=fun((ql,qr),e)->(e::ql,qr)
    let deQ:queue->element*queue=fun(ql,qr)->
      let rec revQ:queue->queue=fun(al,ar)->
        match al with
        |[]->(al,ar)
        |hd::tl->revQ(tl,hd::ar)
      in
        match qr with
        |hd::tl->(hd,(ql,tl))
        |[]->match revQ(ql,qr) with
          |(l,rhd::rtl)->(rhd,(l,rtl))
          |(l,[])->raise EMPTY_Q
  end

