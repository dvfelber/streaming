(* This inserts the numbers 0..7918 into a summary with ep = 1/8; prints the
   ranks of items #2000, #3000, and #5000; prints the numbers at ranks 2000,
   3000, and 5000; and prints approximate 8-quantiles. *)

open Big_int;;
open Streaming;;
open Randomized;;

let q = ref (summary 8 (-1) 7919);;

let () =
  for i = 0 to (7919-1) do
    q := update (!q) (Add ((262*i) mod 7919))
  done ;
  let r2000 = Sketch.rankOf !q 2000
  and r3000 = Sketch.rankOf !q 3000
  and r5000 = Sketch.rankOf !q 5000
  and a2000 = Sketch.atRank !q (big_int_of_int 2000)
  and a3000 = Sketch.atRank !q (big_int_of_int 3000)
  and a5000 = Sketch.atRank !q (big_int_of_int 5000)
  in Printf.printf "rank of 2000: %d\n" (int_of_big_int r2000);
     Printf.printf "rank of 3000: %d\n" (int_of_big_int r3000);
     Printf.printf "rank of 5000: %d\n" (int_of_big_int r5000);
     Printf.printf "at rank 2000: %d\n" a2000;
     Printf.printf "at rank 3000: %d\n" a3000;
     Printf.printf "at rank 5000: %d\n" a5000;
     let qs = Sketch.quantiles !q
     in Printf.printf "quantiles = [";
        for i = 0 to (List.length qs)-1 do
          Printf.printf "%d; " (List.nth qs i)
        done ;
        Printf.printf "]\n";
;;



(* Copyright 2016 David Felber. *)
