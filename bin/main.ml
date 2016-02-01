let () = Pa_macro.linkme
let () = Pa_ounit.linkme
let () = Pa_bench.linkme
(* Pa_type_conv conflicts with Pa_deriving *)
(* let () = Pa_type_conv.linkme *)
let () = Pa_js.linkme
let () = Pa_eliom.linkme
let () = Pa_lwt.linkme
let () = Pa_deriving.linkme

let () = Camlp4_to_ppx.main ()
