(* A quantile summary with (additive, randomized) semantics.

   See: Felber, David, and Rafail Ostrovsky. "A Randomized Online Quantile
        Summary in O (1/epsilon* log (1/epsilon)) Words." LIPIcs-Leibniz
        International Proceedings in Informatics. Vol. 40. Schloss
        Dagstuhl-Leibniz-Zentrum fuer Informatik, 2015.
   Also see the tex file.

   This implementation will give a failure probability of <= 1/2 . That is, it
   fails <= 1/2 of the time. To reduce this probability to near zero, we use the
   median trick in Mediated.ml . *)

open Big_int;;
open Streaming;;
open Util;;

(* Data types. *)

type 'a row =
    {
      iRow: int;                 (* ID of the row, 0 ... *)
      sRow: 'a Quantiles.sketch  (* Internal deterministic summary. *)
    } ;;

type 'a duplicates =
    {
      nCopies: big_int;  (* The total number of times to copy each item. *)
      current: big_int;  (* The number of times left for the current item. *)
      toCopy: 'a list    (* The remaining items to copy.
                            The current item is the head of the list.
                            INVARIANT: ld.toCopy != [] => ls.current > 0. *)
    } ;;

type 'a snapshot =
    {
      pow2i: big_int;                (* 2^i, i = row ID (n_i = 2^i*m) . *)
      snap : 'a Quantiles.snapshot;  (* The underlying snapshot. *)
    } ;;

type 'a sketch =
    {
      lb     : 'a;
      ub     : 'a;
      kError : int;            (* k = 1 / error fraction. *)
      fstRow : 'a row;         (* Current live row. *)
      sndRow : 'a row;
      lastDup: 'a duplicates;  (* Last duplication, for amortization. *)
      nSteps : big_int;        (* Number of steps taken so far. *)
      tNext  : big_int         (* Next transition time. *)
    } ;;



(* *** Scaling functions. *** *)

let inner_error_for_kError k = 4 * k;;

let sample_size_for_kError k = big_int_of_int (4*k*k);;

let max_samples_for_kError k = big_int_of_int (8*k*k);;

let inner_error_to_max_samples kInner = max_samples_for_kError (kInner/4);;

let downsample_quantiles_for_outer qs = every 4 qs;;

let replicates_for_row_i i k = mult_big_int (big_int_of_int k) (power_int_positive_int 2 i);;



(* *** summary (ke:int) (lb:'a) (ub:'a) *** *)

(* Create a summary.
   k  = 1 / (error fraction ep);
          when asked for R(y,X) we return Rhat(y,X) = R(y,X) +- |X|/k .
        We take ke to be a power of two so that it evenly divides m; this
         simplifies the updateB subroutine of the update function below.
   lb = The infimum of the item domain.
   ub = The supremum of the item domain. *)
let summary k lb ub =
  if k < 2 || not (isPowerOf2 k)
  then raise (Summary_exception "Streaming.Quantiles.Randomized.summary: need k = a power of 2 > 1")
  else let s = Quantiles.summary (inner_error_for_kError k) lb ub in
       let r = {iRow = 0; sRow = s}
       and m = sample_size_for_kError k
       and d = {nCopies = zero_big_int; current = zero_big_int; toCopy = []}
       in {lb = lb; ub = ub; kError = k; fstRow = r; sndRow = r; lastDup = d; nSteps = zero_big_int; tNext = m} ;;



(* *** update (q:'a summary) (ch:'a change) *** *)

(* Only insert into the current row's sketch if it hasn't seen too many items. *)
let insertB (r:'a row) (ch:'a change) =
  if lt_big_int (r.sRow.Quantiles.nSteps) (inner_error_to_max_samples r.sRow.Quantiles.kError)
  then { r with sRow = Quantiles.update r.sRow ch }
  else r ;;

(* Get the next item from the duplicates, and update the duplicates to have the
   new item removed. *)
let nextDup ld = match ld.toCopy with
  | x::xs ->
     if eq_big_int ld.current zero_big_int
     then raise (Summary_exception "Streaming.Quantiles.Randomized.nextDup: broken invariant: ld.toCopy != [] implies ls.current > 0")
     else
       (* If there's only one copy left for the current item, go to the next one. *)
       if eq_big_int ld.current unit_big_int
       then x, { ld with current = ld.nCopies; toCopy = xs }
       else x, { ld with current = sub_big_int ld.current unit_big_int }
  | [] -> raise (Summary_exception "Streaming.Quantiles.Randomized.nextDup: ld.toCopy is empty") ;;

(* r  = The row to insert into.
   ld = The lastDup list for amortized insertions.
   ch = The new change to make. *)
let insertNextDup r ld =
  (* Randomly insertB the change or burn it *)
  let pow2i = power_int_positive_int 2 r.iRow in
  let d, ld' = nextDup ld
  and v = random_big_int pow2i in
  let r' = if eq_big_int v zero_big_int
           then insertB r (Add d)
           else r
  in (r',ld') ;;

(* r  = The row to insert into.
   ld = The lastDup list for amortized insertions.
   ch = The new change to make.
   fl = A flag: true to try to insert an item from lastDup. *)
let insert r ld ch fl =
  let i = r.iRow
  in if i == 0                (* If we're on the first row ever *)
     then (insertB r ch, ld)  (* Then always insertB *)
     else
       (* Else only insertB randomly *)
       let pow2i = power_int_positive_int 2 i in
       let v1 = random_big_int pow2i in
       let r1 = if eq_big_int v1 zero_big_int
                then insertB r ch
                else r
       in if fl  (* Then, if we need to add an item from lastDup *)
          then
            (* Then randomly insertB the change or burn it *)
            insertNextDup r1 ld
          else
            (* Else do nothing *)
            (r1,ld) ;;

(* updateB :: (Bounded e, Ord e) => (Sketch e) -> (Sketch e) *)
let updateB s = (* s@(Sketch !ke !rs !lm _ !r _ !ns !tn) = *)
  if eq_big_int s.nSteps s.tNext  (* If we've reached the end of a row *)
  then
    (* Then move the next row up *)
    let repli = replicates_for_row_i s.sndRow.iRow s.kError
    and qtiles = Quantiles.Sketch.quantiles s.fstRow.sRow in
    let ld' = {nCopies = repli; current = repli; toCopy = qtiles}
    and r' = {iRow = (s.sndRow.iRow+1); sRow = Quantiles.summary (inner_error_for_kError s.kError) s.lb s.ub}
    and mul2x = mult_big_int (big_int_of_int 2)
    in { s with fstRow = s.sndRow; sndRow = r'; lastDup = ld'; tNext = mul2x s.tNext }
  else s  (* Else the rows don't change *)
;;

(* Update a summary.
   s  = The summary to update.
   ch = The change to make: Add a copy of x into the summary, or
                            Remove a copy of x from the summary. *)
let update s ch = match ch with
  | Remove _ -> raise (Summary_exception "Streaming.Quantiles.Randomized.Sketch: only additive updates allowed")
  | Add x ->
      let fstRow', lastDup' = insert s.fstRow s.lastDup ch false in
      let sndRow', lastDup'' = insert s.sndRow lastDup' ch true
      in updateB { s with fstRow = fstRow'; sndRow = sndRow'; lastDup = lastDup''; nSteps = add_big_int unit_big_int s.nSteps } ;;



(* *** snap (q:'a summary) *** *)

(* Returns false if nextDup would raise an exception, else true. *)
let hasNextDup ld = match ld.toCopy with
  | x::xs -> gt_big_int ld.current zero_big_int
  | [] -> false ;;

(* Take a snapshot of a summary. *)
let snap s =
  let rec fill r ld =
    if hasNextDup ld
    then let r', ld' = insertNextDup r ld in fill r' ld'
    else r
  and p2i = power_int_positive_int 2 s.fstRow.iRow
  in {pow2i = p2i; snap = Quantiles.snap (fill s.fstRow s.lastDup).sRow} ;;



(* *** Query a snapshot. *** *)

module Snapshot =
  struct
    let rankOf s v =
      let r' = Quantiles.Snapshot.rankOf s.snap v
      in mult_big_int r' s.pow2i
    let atRank s r =
      let r' = div_big_int r s.pow2i
      in Quantiles.Snapshot.atRank s.snap r'
    let quantiles s =
      let qs = Quantiles.Snapshot.quantiles s.snap
      in downsample_quantiles_for_outer qs
  end ;;



(* *** Query a sketch. *** *)

module Sketch =
  struct
    let rankOf s v = Snapshot.rankOf (snap s) v
    let atRank s r = Snapshot.atRank (snap s) r
    let steps s = s.nSteps
    let size s = s.nSteps
    let quantiles s = Snapshot.quantiles (snap s)
  end ;;



(* Copyright 2016 David Felber. *)
