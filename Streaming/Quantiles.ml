(* A (mergeable) quantile summary with (additive, deterministic) semantics.

   See: Munro, J. Ian, and Mike S. Paterson. "Selection and sorting with limited
        storage." Theoretical computer science 12.3 (1980): 315-323.
   And: Agarwal, Pankaj K., et al. "Mergeable summaries." ACM Transactions on
        Database Systems (TODS) 38.4 (2013): 26. *)

open Big_int;;
open Streaming;;
open Util;;

(* Data types. *)

type 'a sVal = { cWgt: big_int; iVal: 'a };;

type 'a block = { weight: int; values: 'a list };;

type 'a snapshot = { lb: 'a; ub: 'a; sError: int; sVals: ('a sVal) array; sMin: 'a; sMax: 'a; sSteps: big_int };;

type 'a sketch = { lb: 'a; ub: 'a; kError: int; blocks: ('a block) list; cBlock: 'a block; qMin: 'a; qMax: 'a; nSteps: big_int };;



(* *** summary (k:int) (lb:'a) (ub:'a) *** *)

(* Create a summary.
   k  = 1 / (error fraction ep);
        when asked for R(y,X) we return Rhat(y,X) = R(y,X) +- |X|/k .
   lb = The infimum of the item domain.
   ub = The supremum of the item domain. *)
let summary k lb ub =
  if k < 2
  then raise (Summary_exception "Streaming.Quantiles.summary: need k > 1")
  else {lb = lb; ub = ub; kError = k; blocks = []; cBlock = {weight = 0; values = []}; qMin = ub; qMax = lb; nSteps = zero_big_int} ;;



(* *** update (q:'a summary) (ch:'a change) *** *)

let insertQ q x =
  let cb = q.cBlock in
  let vs = cb.values in
  let cb' = { cb with values = x :: vs }
  and lv = min x (q.qMin)
  and uv = max x (q.qMax)
  in { q with nSteps = add_big_int q.nSteps unit_big_int; cBlock = cb'; qMin = lv; qMax = uv } ;;

let rec mergeL a b = match a with
  | [] -> b
  | x::xs ->
     match b with
     | [] -> a
     | y::ys ->
        if y < x
        then y :: (mergeL a ys)
        else x :: (mergeL xs b) ;;

let mergeB a b =
  let w = 1 + a.weight
  and v = every 2 (mergeL (a.values) (b.values))
  in {weight = w; values = v} ;;

let rec compressL l = match l with
  | a::b::t ->
     if a.weight == b.weight  (* INVARIANT: weight a <= weight b always. *)
     then compressL ((mergeB a b) :: t)
     else l
  | _ -> l ;;

let compressQ q =
  let cb = q.cBlock in
  let cb' = {cb with values = List.sort compare (cb.values) } in
  let bs = compressL (cb' :: (q.blocks))
  in { q with blocks = bs; cBlock = {weight = 0; values = []} } ;;

(* Update a summary.
   q  = The summary to update.
   ch = The change to make: Add a copy of x into the summary, or
                            Remove a copy of x from the summary. *)
let update q ch = match ch with
  | Remove _ -> raise (Summary_exception "Streaming.Quantiles.Sketch: only additive updates allowed")
  | Add x ->
     let q' = insertQ q x
     in if List.length (q'.cBlock.values) == q'.kError
        then compressQ q'
        else q' ;;



(* *** snap (q:'a summary) *** *)

let rec cwgt c l = match l with
  | [] -> []
  | {cWgt = w; iVal = v}::(xs) ->
     let nw = add_big_int w c
     in {cWgt = nw; iVal = v} :: (cwgt nw xs) ;;

(* Take a snapshot of a summary. *)
let snap q =
  let f b = List.map (fun v -> {cWgt = power_int_positive_int 2 b.weight; iVal = v}) b.values
  and cmp x y = (if x.iVal <= y.iVal then -1 else 1) in
  let sv = cwgt zero_big_int (List.sort cmp (List.concat (List.map f (q.cBlock::q.blocks)))) in
  let la = Array.of_list sv
  in {lb = q.lb; ub = q.ub; sError = q.kError; sVals = la; sMin = q.qMin; sMax = q.qMax; sSteps = q.nSteps} ;;



(* *** Query a snapshot. *** *)

module Snapshot =
  struct
    let rankOf s v =
      let cmp x y = (if x.iVal <= y.iVal then -1 else 1) in
      let i = searchGE s.sVals cmp {cWgt = zero_big_int; iVal = v}
      in if i == -1 then zero_big_int else s.sVals.(i).cWgt
    let atRank (s:'a snapshot) r =
      if lt_big_int r unit_big_int then s.lb
      else if gt_big_int r s.sSteps then s.ub
      else
        let cmp x y = (if le_big_int x.cWgt y.cWgt then -1 else 1) in
        let i = searchLE s.sVals cmp {cWgt = r; iVal = s.lb}
        in if i == -1 then s.lb else s.sVals.(i).iVal
    let minVal s = s.sMin
    let maxVal s = s.sMax
    let steps s = s.sSteps
    let size s = s.sSteps
    let quantiles s =
        (* query s@(Snapshot er sSerror  sv sVals _ _  ss sSteps) _ = *)
      let ei = big_int_of_int s.sError in
      let rs = List.map big_int_of_int (range 1 s.sError) in
      let ad, su, mu, di = add_big_int, sub_big_int, mult_big_int, div_big_int in
      let qs = List.map (fun r -> (di (su (ad (mu s.sSteps r) ei) unit_big_int) ei)) rs
      in List.map (atRank s) qs

  end ;;



(* *** Query a sketch. *** *)

module Sketch =
  struct
    let rankOf q v = Snapshot.rankOf (snap q) v
    let atRank q r = Snapshot.atRank (snap q) r
    let minVal q = q.qMin
    let maxVal q = q.qMax
    let steps q = q.nSteps
    let size q = q.nSteps
    let quantiles q = Snapshot.quantiles (snap q)
  end ;;



(* Copyright 2016 David Felber. *)
