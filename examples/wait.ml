open Shcaml
open UsrBin
open Fitting

let echo s = program "echo" [s];;

let _ = run begin
  ( command "sleep 1"   ^>>
    echo "b"            ^>>
    command "sleep 1" ) ^&= function proc ->
  echo "a"              ^>>
  caml {|
    ignore (Proc.wait proc);
    echo "c"
  |}
end
