(*BEGIN LETOP*)
let (let$) : 'a Lwd.t -> ('a -> 'b) -> 'b Lwd.t = Lwd.Infix.(>|=)
let (and$) : 'a Lwd.t -> 'b Lwd.t -> ('a * 'b) Lwd.t = Lwd.pair
let (let$*) : 'a Lwd.t -> ('a -> 'b Lwd.t) -> 'b Lwd.t = Lwd.Infix.(>>=)
(*END*)

let ($=) : 'a Lwd.var -> 'a -> unit = Lwd.set
let ($<-) : 'a Lwd_table.row -> 'a -> unit = Lwd_table.set

let ( |>$ ) v f = Lwd.map ~f v
let ( >> ) f g x = g (f x)

