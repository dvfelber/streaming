(* This tests some of the functions in Util.ml . *)

open Big_int;;
open Util;;

let l = every 2 [1;2;3;4;5;6;7;8];;
let m = every 2 [1;2;3;4;5;6;7;8;9];;

let z = power_int_positive_int 2 72;;

let r1 = random_big_int z;;
let r2 = random_big_int z;;
let r3 = random_big_int z;;

let () =
  Printf.printf "%30s\n" (string_of_big_int z);
  Printf.printf "%30s\n" (string_of_big_int r1);
  Printf.printf "%30s\n" (string_of_big_int r2);
  Printf.printf "%30s\n" (string_of_big_int r3) ;;

let l = Array.of_list [2;4;5;6;8];;

searchGE l compare 1;;
searchGE l compare 2;;
searchGE l compare 3;;
searchGE l compare 4;;
searchGE l compare 5;;
searchGE l compare 6;;
searchGE l compare 7;;
searchGE l compare 8;;
searchGE l compare 9;;

searchLE l compare 1;;
searchLE l compare 2;;
searchLE l compare 3;;
searchLE l compare 4;;
searchLE l compare 5;;
searchLE l compare 6;;
searchLE l compare 7;;
searchLE l compare 8;;
searchLE l compare 9;;



(* Copyright 2016 David Felber. *)
