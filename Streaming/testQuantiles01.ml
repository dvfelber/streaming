(* This is just a scratchpad to use in manual testing in the top loop. *)

open Big_int;;
open Streaming;;
open Util;;
open Quantiles;;

let q0 = summary 3 1000 0;;

let q1 = update q0 (Add 1);;
let q2 = update q1 (Add 2);;
let q3 = update q2 (Add 3);;

let s3 = snap q3;;

let q4 = update q3 (Add 4);;
let q5 = update q4 (Add 5);;
let q6 = update q5 (Add 6);;

let s6 = snap q6;;

let q7 = update q6 (Add 7);;
let q8 = update q7 (Add 8);;
let q9 = update q8 (Add 9);;

let s9 = snap q9;;



(* Copyright 2016 David Felber. *)
