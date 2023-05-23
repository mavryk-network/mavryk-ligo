open Configurator.V1

let () =
  let f _ = 
    let profile = try Sys.argv.(1) with | _ -> "" in
    let configurator = create "" in
    Flags.write_sexp
      "win32-flags.sexp"
      (if ocaml_config_var_exn configurator "os_type" = "Win32"
       then ["-cclib"; "-lole32"; "-cclib"; "-luserenv"; "-cclib"; "-lbcrypt"; "-cclib"; Sys.getenv "LIGO_NTDLL_PATH" ^ "/ligoNtdll.a"]
       else []);
    Flags.write_sexp
      "macos-link-flags.sexp"
      (if ocaml_config_var_exn configurator "system" = "macosx"
       then ["-cclib"; "-framework Security"]
       else []);
    Flags.write_sexp
      "static-link-flags.sexp"
      (if profile = "static"
       then ["-ccopt"; "-static"; "-cclib"; "-lusb-1.0"; "-cclib"; "-ludev"]
       else [])
  in
  main ~name:"" f
