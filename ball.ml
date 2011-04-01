module type Transducer = sig
  type input
  type output
    
  type abstract
  type concrete

  val init : unit -> abstract

  val embed : abstract -> concrete
  val project : concrete -> abstract

  val step : concrete -> input -> output * concrete
end

type point = { x : int ; y : int ; }
type vector = { dx : int ; dy : int; }
type ball = { radius : int; loc : point ; vel : vector }

type viewport = { width : int ; height : int }

module Impl = struct  
  let sum p v = { x=p.x + v.dx ; y=p.y + v.dy }
  let collide c min max d = if c < min || c > max then -d,true else d,false
 
  type input = viewport 
  type output = unit
  type abstract = ball
  type concrete = ball * int (* collision counter *)

  let init () = 
    { radius = 5; 
      loc = {x=20; y=20} ; 
      vel = {dx=1; dy=1} ;
    }

  let embed b = b,0
  let project (b,c) = b
  
  let collision_duration = 20

  let step (b,c:concrete) (v : viewport) : output * concrete =
    (* update via deltas *)
    let loc' = sum b.loc b.vel in
  
    (* check for collisions *)
    let dx,xc = collide loc'.x b.radius (v.width - b.radius) b.vel.dx in
    let dy,yc = collide loc'.y b.radius (v.height - b.radius) b.vel.dy in
    let vel' = { dx = dx ; dy = dy } in

    (* update collision counter *)
    let c = if xc || yc then collision_duration else c - 1 in

    (* output and new state *)
    (),({ b with loc = loc' ; vel = vel' },c)
  
end

(* force a type check *)
module ImplOK : Transducer = Impl

module GUI = functor 
  (T:Transducer with type input = viewport
                and type output = unit
                and type concrete = ball * int) -> struct
  module G = Graphics
  
  let draw (b,c:ball*int) =
    G.set_color (if c > 0 then G.red else G.black);
    G.fill_circle b.loc.x b.loc.y b.radius
      
  let init () =
    G.open_graph "";
    G.set_window_title "Bouncy!";
    G.auto_synchronize false

  let tick = 0.0075

  let viewport () = 
    { width = G.size_x ();
      height = G.size_y ();
    }

  let rec loop (last:float) (s:T.concrete) =
    let now = Unix.gettimeofday () in
    if G.button_down ()
    then ()
    else if now -. last >= tick
    then begin
      G.clear_graph ();
      draw s;
      G.synchronize ();
      let _,s = T.step s (viewport ()) in
      loop now s
    end
    else loop last s

  let run () = 
    let s = T.embed (T.init ()) in
    loop (Unix.gettimeofday ()) s
end

module App = GUI(Impl)

let _ =
  App.init ();
  App.run ()

