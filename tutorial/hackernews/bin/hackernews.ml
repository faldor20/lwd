
(* Result of part1 *)

(*
   let main_ui =
   W.vbox[
    W.string "hello world"|>Lwd.pure
  ]
*)

(*
   let post_ui ({title; url; score; comments; _} : Hackernews_api.post) =
  let website = List.nth (String.split_on_char '/' url) 2 in
  Ui.vcat
    [ Ui.hcat
        [ W.string ~attr:A.(st bold) title
        ; W.printf ~attr:A.(st italic) "(%s)" website ]
    ; Ui.vcat
        [ W.printf ~attr:A.(st italic) "%d points" score
        ; W.printf ~attr:A.(st italic) "%d comments" comments ] ]
  |> Lwd.pure |> W.BB.focusable
let main_ui =
  let posts = Hackernews_api.fake_posts () in
  posts |> List.map post_ui |> W.vbox
*)

(* Result of part2 *)

(*
   let post_ui ({title; url; score; comments; _} : Hackernews_api.post) =
  let website = List.nth (String.split_on_char '/' url) 2 in
  Ui.vcat
    [ Ui.hcat
        [ W.string ~attr:A.(st bold) title
        ; W.string " "
        ; W.printf ~attr:A.(st italic ++ fg lightblack) "(%s)" website ]
    ; Ui.hcat
        [ W.printf ~attr:A.(st italic) "%d points" score
        ; W.string "  "
        ; W.printf ~attr:A.(st italic) "%d comments" comments ] ]
  |> Ui.resize ~sw:1 ~mw:10000 |> Lwd.pure |> W.Box.focusable

let shortcuts = Ui.vcat [Ui.hcat [W.string "[S]orting"]]

let show_prompt_var = None |> Lwd.var

let sorting_mode_var = Lwd.var `Points

let sorting_prompt ui =
  let open W.Overaly in
  let open W.Lists in
  let res =
    ui
    |> W.Overaly.selection_list_prompt
         ~modify_body:(Lwd.map ~f:(Ui.resize ~sw:1 ~mw:20))
         ~show_prompt_var
    |>$ Ui.keyboard_area (function
          | `ASCII 's', _ ->
              let on_exit x =
                match x with
                | `Closed ->
                    ()
                | `Finished sorting ->
                    sorting_mode_var $= sorting
              in
              let prompt =
                { label= "Sorting method"
                ; items=
                    Lwd.pure
                      [ { data= `Points
                        ; ui= W.Lists.selectable_item (W.string "Points") }
                      ; { data= `Comments
                        ; ui= W.Lists.selectable_item (W.string "Comments") } ]
                ; on_exit }
              in
              show_prompt_var $= Some prompt ;
              `Handled
          | _ ->
              `Unhandled )
  in
  res

let get_sort_func sorting =
  match sorting with
  | `Points ->
      fun a b -> Int.compare b.score a.score
  | `Comments ->
      fun a b -> Int.compare b.comments a.comments

let main_ui =
  let posts =
    let$* sort_mode = Lwd.get sorting_mode_var in
    let sort_func = get_sort_func sort_mode in
    Hackernews_api.fake_posts ()
    |> List.sort sort_func
    |>List.map(post_ui )
    |>W.vbox|>W.Scroll.v_area
  in
  W.vbox [posts|>W.Box.box ~pad_w:1 ~pad_h:0 ; shortcuts|>Ui.resize ~sw:1 ~mw:10000|> Lwd.pure|>W.Box.box ~pad_w:1 ~pad_h:0 ] |> sorting_prompt
*)

let () =
  Nottui.Ui_loop.run  ~quit_on_escape:false Focus_test.main
;;
