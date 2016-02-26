(* Some useful definitions for streaming summaries. *)

exception Summary_exception of string;;

type 'a change = Add of 'a | Remove of 'a;;

(* Copyright 2016 David Felber. *)
