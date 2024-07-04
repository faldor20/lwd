(*open Nottui
  open Lwd_infix

  let selected_item = Lwd.var 0
  let selectable_lwd = Lwd.get selected_item

  let selectable ?(focus = Focus.make ()) id =
  let selected =
  let$ focus = Focus.status focus in
  (*set the focus watcher*)
  let focused = Focus.has_focus focus in
  if focused && Lwd.peek selected_item <> id then Some id else None
  in
  let a =
  let$ focus = Focus.status focus in
  (*set the focus watcher*)
  let focused = Focus.has_focus focus in
  if focused && Lwd.peek selected_item <> id then selected_item $= id;
  W.printf "hi %d focused?:%b" id focused
  |> Ui.keyboard_area ~focus (fun _ -> `Unhandled)
  in
  a, selected
  ;;

  let main =
  let ui, selection = [ 1; 2; 3 ] |> List.map selectable |> List.to_seq |> Seq.unzip in
  let se =
  selection
  |> List.of_seq
  |> Lwd_seq.of_list
  |> Lwd.pure
  |> Lwd_seq.lift
  |> Lwd_seq.fold_monoid
  (fun x -> x)
  (None, fun x y -> if x |> Option.is_some then x else y)
  in
  W.hbox [ W.vbox (ui |> List.of_seq); se |>$ Option.value ~default:0 |>$ W.int ]
  ;;
*)

open Nottui
open Bonsai
open Bonsai.Let_syntax

let main graph =
  let selected, set_selected = Bonsai.state 0 graph in
  let selectable ?(focus = Focus.make ()) id =
    let focus_status = Focus.status focus in
    Bonsai.Edge.on_change
      focus_status
      graph
      ~equal:(fun x y -> Focus.has_focus x == Focus.has_focus y)
      ~callback:
        (let%arr set_selected = set_selected in
         fun focus ->
           (*set the focus watcher*)
           let focused = Focus.has_focus focus in
           if focused then set_selected id else Effect.return ());
    let%arr focus = focus_status in
    Notty.I.strf "hi from bonsai!! %d focused?:%b" id (focus |> Focus.has_focus)
    |> Ui.atom
    |> Ui.keyboard_area ~focus (fun _ -> None,`Unhandled)
  in
  let show =
    selected |> Bonsai.map ~f:(fun x -> x |> Notty.I.strf "selected:%i" |> Ui.atom)
  in
  W.hbox [ [ 1; 2; 3 ] |> List.map selectable |> W.vbox; show ]
;;
