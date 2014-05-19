open Ocamlbuild_plugin

let root_dir = Sys.getcwd ()

let pp_linespec = Filename.concat (Sys.getcwd ()) "pp_linespec.byte"

(* not sure why I have to fucking do this... *)
let camlp4 = (Findlib.query "camlp4").Findlib.location

let () = rule "cppo" ~dep:"%.ml.cpp" ~prod:"%.ml" begin fun env _build ->
  Cmd (S[
    A "cppo"; 
    S [A "-I"; P (Filename.concat root_dir "lib")];
    A "-n";
    S [A "-o"; P (env "%.ml");];
    P (env "%.ml.cpp");
  ])
end

let () =
  rule "linespec: lineMetadata special rule ffs"
    ~deps:["lib/line.ls"; "syntax/pp_linespec.byte"]
    ~prod:"syntax/lineMetadata.ml"
    begin fun env _build ->
      let p = env "lib/line.ls" in
      let pp_linespec = env "syntax/pp_linespec.byte" in
      Cmd(S[
        A pp_linespec;
        A "-data";
        P p;
        Sh ">";
        P (env "syntax/lineMetadata.ml")
      ])
    end

let () =
  rule "linespec: ls -> ml"
    ~deps:["%.ls"; "syntax/pp_linespec.byte"]
    ~prods:["%.mli"; "%.ml"]
    begin fun env _build ->
      let p = env "%.ls" in
      let pp_linespec = env "syntax/pp_linespec.byte" in
      let cmd flag target =
        Cmd(S[
          A pp_linespec;
          A flag;
          P p;
          Sh ">";
          P (env target)
        ]) in
      Seq [cmd "-impl" "%.ml"; cmd "-intf" "%.mli"]
    end

let () = 
  dispatch begin function 
  | After_rules ->
    ocaml_lib ~extern:true ~dir:camlp4 "camlp4of";
    pflag ["ocaml";"compile";] "define" (fun s -> S [A"-ppopt"; A (s)]);
    pflag ["ocaml";"ocamldep";] "define" (fun s -> S [A"-ppopt"; A (s)])
  | _ -> ()
  end
