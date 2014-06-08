(** Shtreams of strings.
 * This module is the result of applying {!AnyShtream.Make} to the
 * module {!StringElem}.
 * Thus, shtreams handled by this module are compatible with the shtreams
 * of {!Shtream}, {!LineShtream}, and modules created by
 * {!AnyShtream.Make}, but this module provides additional functions
 * for reading and writing [string Stream.t]s.
 *)

(** The parameter given to {!AnyShtream.Make} to build this module. *)
module StringElem : S.ELEM
  with type 'a elem = string
   and type initial = unit

(** The real contents of {!StringShtream}. *)
include AnyShtream.ANYSHTREAM with module Elem = StringElem
