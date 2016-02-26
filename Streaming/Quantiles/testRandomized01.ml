(* This is just a scratchpad to use in manual testing in the top loop. *)

open Big_int;;
open Streaming;;
open Randomized;;

let i = ref 0;;
let s = ref (summary 4 (-1) 1009);;

s := update (!s) (Add ((262*(!i)) mod 1009));;
i := (!i) + 1;;
s;;

let () =
  i := 0;
  while !i < 256 do
    s := update (!s) (Add ((262*(!i)) mod 1009));
    i := !i + 1
  done ;;
Sketch.quantiles !s;;
int_of_big_int !s.nSteps, int_of_big_int !s.tNext;;

let () =
  i := 256;
  while !i < 512 do
    s := update (!s) (Add ((262*(!i)) mod 1009));
    i := !i + 1
  done ;;
Sketch.quantiles !s;;
int_of_big_int !s.nSteps, int_of_big_int !s.tNext;;

let () =
  i := 512;
  while !i < 1009 do
    s := update (!s) (Add ((262*(!i)) mod 1009));
    i := !i + 1
  done ;;
Sketch.quantiles !s;;
int_of_big_int !s.nSteps, int_of_big_int !s.tNext;;



let d = ref (-1);;
let ld = ref {nCopies = big_int_of_int 3; current = big_int_of_int 3; toCopy = [4;5;6;7]};;

let d', ld' = nextDup !ld in d := d'; ld := ld'; (d,ld);;



(* Copyright 2016 David Felber. *)
