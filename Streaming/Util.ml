(* Utility functions for quantile summaries. *)

open Big_int;;
open Streaming;;

(* *** drop (n:int) (xs:'a list)
       every (n:int) (xs:'a list)
       replicate (n:int) (x:'a)
       range (a:int) (b:int)
       transpose (xs:'a list list) *** *)

(* Drop the first n items. *)
let rec drop n xs =
  if n <= 0 then xs else
    match xs with
    | [] -> []
    | y::ys -> drop (n-1) ys ;;

(* Return every nth item.
   From: https://stackoverflow.com/questions/2026912/how-to-get-every-nth-
         element-of-an-infinite-list-in-haskell (16 Sep 2015) *)
let rec every n xs = match drop (n-1) xs with
  | [] -> []
  | y::ys -> y :: every n ys ;;

(* Replicate an item n times. *)
let replicate n x =
  let rec replicateB n' z = if n' <= 0 then z else replicateB (n'-1) (x::z)
  in List.rev (replicateB n []) ;;

(* Returns the range [a, a+1, ..., b-1, b]. *)
let range a b =
  let rec rangeB a b z = if b < a then z else rangeB (a+1) b (a::z)
  in List.rev (rangeB a b []) ;;

(* Transposes a list of lists.
   NOTE: This implementation isn't too efficient. *)
let transpose xs =
  if xs == []
  then raise (Summary_exception "Streaming.Util.transpose: empty list of lists")
  else
    if (List.nth xs 0) == []
    then raise (Summary_exception "Streaming.Util.transpose: xs has an empty list")
    else
      let checkLengths () =
        let rec checkLengthsB xs l = match xs with
          | [] -> ()
          | (y::ys) ->
             if l != List.length y
             then raise (Summary_exception "Streaming.Util.transpose: lists have different lengths")
             else checkLengthsB ys l
        in checkLengthsB xs (List.length (List.nth xs 0))
      and heads xs = List.map List.hd xs
      and tails xs = List.map List.tl xs
      in let rec transposeB us zs = match (List.nth us 0) with
           | [] -> zs
           | (v::vs) -> transposeB (tails us) ((heads us)::zs)
         in checkLengths ();
            List.rev (transposeB xs []) ;;



(* *** isPowerOf2 (ke:int) *** *)

(* Boolean, whether ke is a power of 2. *)
let rec isPowerOf2 ke =
  if ke < 1 then false
  else if ke == 1 then true
  else if ke mod 2 == 1 then false
  else isPowerOf2 (ke / 2) ;;



(* *** searchGE (a:'a array) (cmp:'a -> 'a -> bool) (v:'a)
       searchLE (a:'a array) (cmp:'a -> 'a -> bool) (v:'a) *** *)

let rec searchB basecase cmpv a lb ub =
  if ub == lb
  then basecase a ub
  else
    let m = (lb+ub) / 2
    and re = searchB basecase cmpv a
       in if cmpv a.(m) == 1 then re (m+1) ub else re lb m ;;

let search basecase outer a cmp v =
  let lb = 0
  and ub = (Array.length a) - 1
  and cmpv = cmp v
  in if ub < lb || outer a lb ub
     then lb-1
     else searchB basecase cmpv a lb ub ;;

(* Returns the last index i in sorted array a for which
   (cmp v (a.(i))) == 0 or 1. *)

let searchGE a cmp v =
  let basecase a ub = if cmp v a.(ub) == -1 then ub-1 else ub
  and outer a lb ub = cmp v a.(lb) == -1
  in search basecase outer a cmp v ;;

(* Returns the first index in sorted array a for which
   (cmp v (a.(i))) == -1 or 0. *)
let searchLE a cmp v =
  let basecase a ub = ub
  and outer a lb ub = cmp v a.(ub) == 1
  in search basecase outer a cmp v ;;



(* *** random_big_int (z:big_int) -> big_int *** *)

(* Returns a big_int in the range [0,z), where
     2^(30*c) <= z < 2^(30*(c+1)) and
     2^d      <= z < 2^(d+1)      .
   May be useful separate from random_big_int . *)
let random_big_int_bounded z c d =
  if le_big_int z zero_big_int
  then failwith "Unit.random_big_int: need z > 0"
  else
    if eq_big_int z unit_big_int
    then zero_big_int
    else
      let u30 = power_int_positive_int 2 30 in
      let d2 = power_int_positive_int 2 d in
      let rec f a u =
        if u == 0
        then
          let am = mod_big_int a d2 in
          if gt_big_int am z then f zero_big_int c else am
        else f (add_big_int (mult_big_int a u30) (big_int_of_int (Random.bits ()))) (u-1)
      in f zero_big_int c;;

(* Generates bounds c, d for z.
   May be useful separate from random_big_int . *)
let random_big_int_get_bounds z =
  if le_big_int z unit_big_int
  then (1,1)
  else
    let u30 = power_int_positive_int 2 30
    and two = big_int_of_int 2 in
    let rec cdf zc df uc =
      if eq_big_int zc zero_big_int
      then uc
      else cdf (div_big_int zc df) df (uc+1) in
    let c = cdf z u30 0
    and d = cdf z two 0
    in (c,d) ;;

(* Returns a big_int in the range [0,z). *)
let random_big_int z =
  let (c,d) = random_big_int_get_bounds z
  in random_big_int_bounded z c d ;;



(* Copyright 2016 David Felber. *)
