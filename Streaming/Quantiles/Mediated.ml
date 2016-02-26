(* A quantile summary with (additive, randomized) semantics.

   See Randomized.ml . The implementation in this file uses that one, except we
   use several copies in parallel and take the median on queries, so that we get
   any small failure probability delta instead of 1/2 . *)

open Big_int;;
open Streaming;;
open Util;;

(* Data types. *)

type 'a sketch =
    {
      pError : float;                      (* Probability of error on a query. 0 = no error. *)
      copies : 'a Randomized.sketch list;  (* The copies. *)
      compare: ('a -> 'a -> int)           (* A comparison function for 'a types,
                                              so that we can define the median. *)
    } ;;



(* *** summary (ke:int) (lb:'a) (ub:'a) *** *)

let cError pe =
  let scale = (log (1.0 /. pe)) /. (log 2.0)
  in 1 + 2 * (int_of_float (ceil scale)) ;;

(* Create a summary.
   k   = 1 / (error fraction ep);
           when asked for R(y,X) we return Rhat(y,X) = R(y,X) +- |X|/k .
         We take ke to be a power of two so that it evenly divides m.
   lb  = The infimum of the item domain.
   ub  = The supremum of the item domain.
   pe  = Probability of failure, in the range 0 < pe < 1 .
   cmp = A comparison function for the item types.
*)
let summary k lb ub pe cmp =
  if k < 2 || not (isPowerOf2 k)
  then raise (Summary_exception "Streaming.Quantiles.Mediated.summary: need k = a power of 2 > 1")
  else
    if pe < 0.0 || pe > 1.0
    then raise (Summary_exception "Streaming.Quantiles.Mediated.summary: need 0.0 < pe < 1.0")
    else
      let copy x = Randomized.summary k lb ub in
      let cps = List.map copy (range 0 (cError pe))
      in {pError = pe; copies = cps; compare = cmp} ;;



(* *** update (q:'a summary) (ch:'a change) *** *)

(* Update a summary.
   s  = The summary to update.
   ch = The change to make: Add a copy of x into the summary, or
                            Remove a copy of x from the summary. *)
let update s ch = { s with copies = List.map (fun s' -> Randomized.update s' ch) s.copies } ;;



(* *** Query a sketch. *** *)

(* Get the median of a list.
   This could be made O(n), but we won't. *)
let median cmp l =
  let sorted = List.sort cmp l
  and medianIndex = (List.length l) / 2
  in (List.nth sorted medianIndex) ;;

module Sketch =
  struct
    let rankOf s v = median compare_big_int (List.map (fun q -> Randomized.Sketch.rankOf q v) s.copies)
    let atRank s r = median s.compare (List.map (fun q -> Randomized.Sketch.atRank q r) s.copies)
    let steps s = (List.hd s.copies).Randomized.nSteps
    let size s = (List.hd s.copies).Randomized.nSteps
    let quantiles s =
      let qs = List.map Randomized.Sketch.quantiles s.copies
      in List.map (median s.compare) (transpose qs)
  end ;;



(* Copyright 2016 David Felber. *)
