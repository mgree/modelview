
type point = { x : int ; y : int ; }
type vector = { dx : int ; dy : int; }
type ball = { radius : int; loc : point ; vel : vector }

let bound b width height =
  let l = b.loc in
  let x = max (min (width-b.radius) l.x) b.radius in
  let y = max (min (height-b.radius) l.y) b.radius in
  { b with loc = { x = x; y = y} }

module type WORLD = sig
  val update : ball -> int -> int -> ball
end

module type Transducer = sig
  type abstract
  type concrete

  val initial : abstract
    (** A seed value for the computation. *)

  val wake : unit -> unit
    (** Send a notice to the transducer that it is being restored after a transfer. *)

  val embed : abstract -> concrete
  val project : concrete -> abstract

  val step : concrete -> concrete

end

module World = struct
  let sum p v = { x=p.x + v.dx ; y=p.y + v.dy }
  let collide c min max d = if c < min || c > max then -d else d

  let update (b : ball) width height : ball =
    (* update via deltas *)
    let loc' = sum b.loc b.vel in

    (* check for collisions *)
    let vel' = {
      dx = collide loc'.x b.radius (width - b.radius) b.vel.dx ;
      dy = collide loc'.y b.radius (height - b.radius) b.vel.dy ;
    } in
    { b with loc = loc' ; vel = vel' }

end

let init_graphics () =
  Graphics.open_graph "";
  Graphics.set_window_title "Bouncy!";
  Graphics.auto_synchronize false

module GUI
  (M: sig val width: int
          val height: int
          val origin: point
          module W: WORLD end) :  Transducer with type abstract = ball =
struct
  module G = Graphics

  type abstract = ball
  type concrete = {last : float; ball : ball}

  let initial = { radius = 5; loc = {x=20; y=20} ; vel = {dx=5; dy=5} }

  let draw (b:ball) =
    G.draw_rect (M.origin.x-b.radius) (M.origin.y-b.radius) (M.width+b.radius) (M.height+b.radius);
    G.fill_circle (M.origin.x + b.loc.x) (M.origin.y + b.loc.y) b.radius

  let wake () = ()

  let tick = 0.03125

  let step ({last=last; ball=b} as state) =
    let now = Unix.gettimeofday () in
    if now -. last >= tick
    then begin
      G.clear_graph ();
      draw b;
      G.synchronize ();
      let b = M.W.update b M.width M.height in
      {last=now; ball=b}
    end
    else state

  let embed (b:ball) = {last = Unix.gettimeofday (); ball=bound b M.width M.height}
  let project x = x.ball
end

module Make (T: Transducer) = struct
  let wake = T.wake

  type abstract = T.abstract

  let v0 : abstract = T.initial

  let start (a : abstract) : abstract =
    let rec loop s =
      if Graphics.key_pressed () 
      then (ignore (Graphics.read_key ()); T.project s)
      else let s = T.step s in loop s in
    loop (T.embed a)
end

module App1 = Make(GUI(struct let width = 100 let height = 70 let origin = {x=30; y=30}; module W = World end))
module App2 = Make(GUI(struct let width = 100 let height = 300 let origin = {x=150; y=30}; module W = World end))

let main () =
  init_graphics ();
  let rec loop v =
    let v = App1.start v in
    let v = App2.start v in
    loop v in
  loop App1.v0
;;

main ()
