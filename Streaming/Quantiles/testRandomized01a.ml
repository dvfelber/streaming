(* This is just a scratchpad to use in manual testing in the top loop. *)

open Big_int;;
open Streaming;;
open Randomized;;



let updates s a b =
  for i = a to (b-1) do
    s := update (!s) (Add ((262*i) mod 7919))
  done ;;

let blank () = Printf.printf "\n";;

let examples s =
  let r2000 = Sketch.rankOf !s 2000
  and r3000 = Sketch.rankOf !s 3000
  and r5000 = Sketch.rankOf !s 5000
  and a2000 = Sketch.atRank !s (big_int_of_int 2000)
  and a3000 = Sketch.atRank !s (big_int_of_int 3000)
  and a5000 = Sketch.atRank !s (big_int_of_int 5000)
  in Printf.printf "r2000: %d\n" (int_of_big_int r2000);
     Printf.printf "r3000: %d\n" (int_of_big_int r3000);
     Printf.printf "r5000: %d\n" (int_of_big_int r5000);
     Printf.printf "@2000: %d\n" a2000;
     Printf.printf "@3000: %d\n" a3000;
     Printf.printf "@5000: %d\n" a5000 ;;

let currentsnap s =
  let t = Quantiles.snap !s.fstRow.sRow
  in Printf.printf "snap min,max,steps = %d,%d,%d\n" t.Quantiles.sMin t.Quantiles.sMax (int_of_big_int t.Quantiles.sSteps);
     Printf.printf "snap sVals:\n";
     for i = 0 to ((Array.length t.Quantiles.sVals)-1) do
       Printf.printf "{cWgt = %6d, iVal = %6d}\n" (int_of_big_int t.Quantiles.sVals.(i).Quantiles.cWgt) t.Quantiles.sVals.(i).Quantiles.iVal
     done ;;

let qtiles s =
  let qs = Sketch.quantiles !s
  in Printf.printf "quantiles = [";
     for i = 0 to (List.length qs)-1 do
       Printf.printf "%d; " (List.nth qs i)
     done ;
     Printf.printf "]\n" ;;

let run s a b =
  updates s a b;
  examples s;
  blank ();
  currentsnap s;
  blank ();
  Printf.printf "s.nSteps,tNext = %d, %d\n" (int_of_big_int !s.nSteps) (int_of_big_int !s.tNext);
  blank ();
  Printf.printf "s.lastDup.nCopies = %d\n" (int_of_big_int !s.lastDup.nCopies);
  Printf.printf "s.lastDup.todo    = %d\n" ((int_of_big_int !s.lastDup.current) + (int_of_big_int !s.lastDup.nCopies)*(List.length (List.tl !s.lastDup.toCopy)));
  blank ();
  qtiles s;
  blank ();
  blank () ;;



let s = ref (summary 8 (-1) 7919);;
blank ();;
currentsnap s;;
blank ();;
Printf.printf "s.nSteps,tNext = %d, %d\n" (int_of_big_int !s.nSteps) (int_of_big_int !s.tNext);
blank ();
Printf.printf "s.lastDup.nCopies = %d\n" (int_of_big_int !s.lastDup.nCopies);;
Printf.printf "s.lastDup.todo    = %d\n" ((int_of_big_int !s.lastDup.current) + (int_of_big_int !s.lastDup.nCopies)*(List.length (List.tl !s.lastDup.toCopy)));;
blank ();;
blank ();;
run s 0 512;;
run s 512 1023;;
run s 1023 1024;;



(* Copyright 2016 David Felber. *)
