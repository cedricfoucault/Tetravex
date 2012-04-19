(** This module defines the views for the Tetravex application.
  
  It handles the queries from the user, 
  arranges and displays the views for the application
  and interfaces the models with the views
  
  @author "Cedric Foucault".
*)

(**  *)
module Window : sig
  type t = Widget.toplevel Widget.widget
  val create : int -> int -> t (* Widget.toplevel Widget.widget *)
  val resize : t -> int -> int -> unit
  val close : unit -> unit
end

module Menu : sig
  type t = Widget.frame Widget.widget
  val create :
    newgamecallback:(int -> int -> unit) ->
    solvecallback:(unit -> unit) ->
    opencallback:(string -> unit) ->
    savecallback:(string -> unit) ->
    width:int -> 
    rows:int -> columns:int ->
    'a Widget.widget -> t (* Widget.frame Widget.widget *)
end

module TileView : sig
  type t = Widget.canvas Widget.widget
  val create :
    model:Tetravex.Tile.t -> 
    ?width:int ->
    ?height:int ->
    'a Widget.widget -> t (* Widget.canvas Widget.widget *)
end

exception No_Tile

class puzzle_grid :
  model:Tetravex.Puzzle.t -> 
  ?width:int ->
  ?height:int ->
  'a Widget.widget ->
  object
    method get_frame : Widget.frame Widget.widget
    (* method get_tiles : Widget.canvas Widget.widget array array *)
    method locate_tile : int -> int -> int * int
    (* method update_tiles : unit -> unit *)
    method update : unit -> unit
    method bind_drag_start : (
      row:int -> col:int ->
      model:Tetravex.Tile.t ->
      view:TileView.t ->
      Tk.eventInfo -> unit
      ) -> unit
    method bind_drag_move : (
      model:Tetravex.Tile.t ->
      view:TileView.t ->
      Tk.eventInfo -> unit
      ) -> unit
    method bind_drag_stop : (
      model:Tetravex.Tile.t ->
      view:TileView.t ->
      Tk.eventInfo -> unit
      ) -> unit
    method destroy : unit -> unit
  end
  
class tile_set :
  model:Tetravex.Puzzle.t ->
  ?width:int ->
  ?height:int ->
  ?x:int -> ?y:int ->
  'a Widget.widget ->
  object
    (* model x y  *)
    method add : model:Tetravex.Tile.t -> x:int -> y:int -> unit -> int
    method place : model:Tetravex.Tile.t -> view:TileView.t -> 
      x:int -> y:int -> unit -> int
    method remove : id:int -> unit -> unit
    (* id model view event *)
    method bind_drag_start : 
      (id:int -> model:Tetravex.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
    method bind_drag_move : 
      (id:int -> model:Tetravex.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
    method bind_drag_stop :
      (id:int -> model:Tetravex.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
    method fire_drag_start : id:int -> Tk.eventInfo -> unit
    (* id x y *)
    method update_pos : id:int -> x:int -> y:int -> unit -> unit
    method update : unit -> unit
    method destroy : unit -> unit
  end
  