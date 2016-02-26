(* This inserts the numbers 0..1008 into a summary with ep = 1/8; prints the
   numbers at ranks 300, 600, and 900; prints the blocks in the resulting
   snapshot; and prints approximate 5-quantiles. *)

open Big_int;;
open Streaming;;
open Util;;
open Quantiles;;

let q = ref (summary 5 (-1) 1009);;

let () =
  for i = 0 to (1009-1) do
    q := update (!q) (Add ((262*i) mod 1009))
  done ;
  let s = snap !q in
  let a300 = Snapshot.atRank s (big_int_of_int 300)
  and a600 = Snapshot.atRank s (big_int_of_int 600)
  and a900 = Snapshot.atRank s (big_int_of_int 900)
  in Printf.printf "@300: %d\n" a300;
     Printf.printf "@600: %d\n" a600;
     Printf.printf "@900: %d\n" a900;
     Printf.printf "snap min,max,steps = %d,%d,%d\n" s.sMin s.sMax (int_of_big_int s.sSteps);
     Printf.printf "snap sVals:\n";
     for i = 0 to ((Array.length s.sVals)-1) do
       Printf.printf "{cWgt = %6d, iVal = %6d}\n" (int_of_big_int s.sVals.(i).cWgt) s.sVals.(i).iVal
     done ;
     let qs = Snapshot.quantiles s
     in Printf.printf "quantiles = [";
        for i = 0 to (List.length qs)-1 do
          Printf.printf "%d; " (List.nth qs i)
        done ;
        Printf.printf "]\n";
;;



(* Copyright 2016 David Felber. *)
