include Nottui_main

module Utils=struct

type 'a monoid = 'a * ('a -> 'a -> 'a)

let lift_monoid (zero, plus) =
  (Bonsai.return zero, Bonsai.map2 ~f:plus)

let map_reduce inj (zero, plus) items =
  let rec cons_monoid c xs v =
    match xs with
    | (c', v') :: xs when c = c' ->
      cons_monoid (c + 1) xs (plus v' v)
    | xs -> (c, v) :: xs
  in
  let cons_monoid xs v = cons_monoid 0 xs (inj v) in
  match List.fold_left cons_monoid [] items with
  | [] -> zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> plus v acc) x xs

let reduce monoid items = map_reduce (fun x -> x) monoid items

let rec cons_lwd_monoid plus c xs v =
  match xs with
  | (c', v') :: xs when c = c' ->
    cons_lwd_monoid plus (c + 1) xs (Bonsai.map2 ~f:plus v' v)
  | xs -> (c, v) :: xs

let pack (zero, plus) items =
  match List.fold_left (cons_lwd_monoid plus 0) [] items with
  | [] -> Bonsai.return zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> Bonsai.map2 ~f:plus v acc) x xs

let pack_seq (zero, plus) items =
  match Seq.fold_left (cons_lwd_monoid plus 0) [] items with
  | [] -> Bonsai.return zero
  | (_,x) :: xs ->
    List.fold_left (fun acc (_, v) -> Bonsai.map2 ~f:plus v acc) x xs

let rec map_l (f:'a -> 'b Bonsai.t) (l:'a list) : 'b list Bonsai.t =
  match l with
  | [] -> Bonsai.return []
  | x :: tl -> Bonsai.map2 ~f:List.cons (f x) (map_l f tl)

let flatten_l (l:'a Bonsai.t list) : 'a list Bonsai.t =
  map_l (fun x->x) l

(** {1 Miscellaneous functions}

    I don't know where to put these, but they are useful, especially for
    UI-related computations.
*)

let mini a b : int = if b < a then b else a

let maxi a b : int = if b > a then b else a

let clampi x ~min ~max : int =
  if x < min then
    min
  else if x > max then
    max
  else
    x

let minf a b : float = if b < a then b else a

let maxf a b : float = if b > a then b else a

let clampf x ~min ~max : float =
  if x < min then
    min
  else if x > max then
    max
  else
    x
end

module W= struct
  open Nottui_main 

let hbox l = Utils.pack Ui.pack_x l
let vbox l = Utils.pack Ui.pack_y l
let zbox l = Utils.pack Ui.pack_z l
end
