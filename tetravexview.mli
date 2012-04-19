(** This module defines the views for the Tetravex application.
  
  It provides functions to create visual representations of
  the models, ways to update them, and defines the style of
  the GUI (Graphical User Interface).
*)

(** The view for the main application window. *)
module Window : sig
  (** The type of the window. *)
  type t = Widget.toplevel Widget.widget
  
  (** [Window.create w h] creates the application window 
  with resolution [w * h]. *)
  val create : int -> int -> t (* Widget.toplevel Widget.widget *)
  
  (** [Window.resize win w h] resizes the window [win]
  to width [w] and height [h]. *)
  val resize : t -> int -> int -> unit
  
  (** Closes the application window. *)
  val close : unit -> unit
end

(** The view for the GUI main menu. *)
module Menu : sig
  (** The type of the menu. *)
  type t = Widget.frame Widget.widget
  
  (** Creates a menu and bind the button actions with the given callbacks.
  [newgamecallback m n] is called when the user clicks on the
  "New Game" button with entries [n] x [m].
  [solvecallback] is called when the user clicks on "Get Solution".
  [opencallback path] is called when the user opens a file
  by clicking on the "Open" button.
  [savecallback filepath] is called when the user enters a file path
  by clicking on the "Save" button.
  The last argument is the parent of the menu (usually the main window). *)
  val create :
    newgamecallback:(int -> int -> unit) ->
    solvecallback:(unit -> unit) ->
    opencallback:(string -> unit) ->
    savecallback:(string -> unit) ->
    width:int -> 
    rows:int -> columns:int ->
    'a Widget.widget -> t (* Widget.frame Widget.widget *)
end

(** The views for a Tetravex tile. *)
module TileView : sig
  (** The type of the view. *)
  type t = Widget.canvas Widget.widget
  
  (** Creates a view for the given [model] of a tile 
  with the given widget parent. Default [height] and [width] is 60. *)
  val create :
    model:TetravexModel.Tile.t -> 
    ?width:int ->
    ?height:int ->
    'a Widget.widget -> t (* Widget.canvas Widget.widget *)
end

exception No_Tile

(** A view of a Tetravex puzzle in the form of a grid of tiles.
  [width] and [height] are the dimensions of the tiles (default: 60). *)
class puzzle_grid :
  model:TetravexModel.Puzzle.t -> 
  ?width:int ->
  ?height:int ->
  'a Widget.widget ->
  object
    (** Returns the frame of the grid. *)
    method get_frame : Widget.frame Widget.widget
    
    (** [locate_tile x y] returns the indices on the grid 
    of the tile located at [(x, y)] 
    (raises [exception No_Tile] if outside of grid).*)
    method locate_tile : int -> int -> int * int
    
    (** Updates the view based on the given [model]. *)
    method update : unit -> unit
    
    (** Binds a mouse click-pressed callback to each tile view on the grid. *)
    method bind_drag_start : (
      row:int -> col:int ->
      model:TetravexModel.Tile.t ->
      view:TileView.t ->
      Tk.eventInfo -> unit
      ) -> unit
      
    (** Destroys all the views associated to the grid (tiles and frame). *)
    method destroy : unit -> unit
  end
  
(** A view of a Tetravex puzzle in the form of a set of tiles
  that can be placed independently of each other.
  [width] and [height] are the dimensions of the tiles (default: 60). *)
class tile_set :
  model:TetravexModel.Puzzle.t ->
  ?width:int ->
  ?height:int ->
  ?x:int -> ?y:int ->
  'a Widget.widget ->
  object
    (* Adds a tile view from the given [model],
    placed at the given absolute location [(x, y)].
    The integer returned by the method is the ID of the newly created tile
    (each tile has a unique ID in the set to identify them). *)
    method add : model:TetravexModel.Tile.t -> x:int -> y:int -> unit -> int
    
    (** Removes the tile that has the given [id]. *)
    method remove : id:int -> unit -> unit
    
    (** Binds a mouse click-pressed
     action callback to each tile view in the set. *)
    method bind_drag_start : 
      (id:int -> model:TetravexModel.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
         
    (** Binds a mouse-motion callback to each tile view in the set.*)
    method bind_drag_move : 
      (id:int -> model:TetravexModel.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
         
    (** Binds a mouse click-release callback to each tile view in the set. *)
    method bind_drag_stop :
      (id:int -> model:TetravexModel.Tile.t -> view:TileView.t -> 
         Tk.eventInfo -> unit) -> unit
         
    (** Forces a call to the drag start callback with the given event info 
      of the tile that has the given [id]. *)
    method fire_drag_start : id:int -> Tk.eventInfo -> unit
    
    (** Updates the position of the tile that has the given [id]
    to the given coordinates [(x, y)]. *)
    method update_pos : id:int -> x:int -> y:int -> unit -> unit
    
    (** Updates the views based on the current coordinates. *)
    method update : unit -> unit
    
    (** Destroy all the views associated to the set. *)
    method destroy : unit -> unit
  end
  
(** @author "Cedric Foucault" *)
  