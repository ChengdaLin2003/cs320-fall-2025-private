open Interp3

let _ =
  if Array.length Sys.argv > 1
  then
    let filename = Sys.argv.(1) in
    match In_channel.(input_all (open_in filename)) with
    | input -> (
        match interp input with
        | Error e -> print_endline ("error: " ^ err_msg e)
        | Ok _ -> ()
      )
    | exception _ ->
      print_endline ("error: could not read \'" ^ filename ^ "\'")
  else
    print_endline "error: no file given"
