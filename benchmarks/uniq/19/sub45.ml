let uniq : 'a list -> 'a list
= fun lst -> (* TODO *);;
const reverseMerge = flip(merge);

export function fetchSuccess(state, { payload: { gators } }) {
   const gatorIds = map(prop("id"), gators);
   const gatorLookup = indexBy(prop("id"), gators);

   return evolve(
     {
       all: compose(uniq, concat(gatorIds)),
       byId: reverseMerge(gatorLookup),
       loading: always(false)
     },
     state
   );
 }