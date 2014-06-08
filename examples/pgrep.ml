open Shcaml
open UsrBin
open Fitting
open Util

let pgrep pat = ignore ^$ run begin
  ps () -|
  grep (Reader.starts_with pat % Line.Ps.command) -|
  cut (string_of_int % Line.Ps.pid)
end

;;
let _ = if Array.length Sys.argv > 1 then
    pgrep Sys.argv.(1)
  else
    Printf.eprintf "Usage: %s PROGRAM\n"  Sys.argv.(0)
