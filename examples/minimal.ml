open Nottui
open! Bonsai
open Bonsai.Let_syntax
open Notty

(* Put the UI here *)

let counter graph =
  let count, set_count = Bonsai.state 0 graph in
  let view =
    let%arr count = count
    and set_count = set_count in
    (* view-construction logic *)
    let count_ui =
      Ui.atom (Notty.I.string A.empty [%string "Counter value: %{count#Int}"])
      |> Ui.keyboard_area (function
        | `Enter, _ -> Some (set_count 10), `Handled
        | _ -> None, `Unhandled)
    in
    count_ui
  in
  view
;;

let root = counter
let () = Ui_loop.run ~tick_period:0.2 root
