(** This module defines the controller for the Tetravex application.
  
  It handles the queries from the user, 
  arranges and displays the views for the application
  and interfaces the models with the views
  
  @author "Cedric Foucault".
*)

(** Starts the application. *)
val start : ?resolution:(int * int) -> unit -> unit

(** Contains the top level window for the current game. *)
val main_window : Tetravexview.Window.t ref

(** Contains the model of the current puzzle that is to solve. *)
val tile_set_model : Tetravex.Puzzle.t ref

(** Contains the view of the current remaining set of tiles. *)
val tile_set_view : Tetravexview.tile_set ref

(** Contains the model of the current, partially filled puzzle grid. *)
val puzzle_grid_model : Tetravex.Puzzle.t ref

(** Contains the view of the current, partially filled puzzle grid. *)
val puzzle_grid_view : Tetravexview.puzzle_grid ref

(** Creates the menu for the current game. *)
val make_menu : unit -> Tetravexview.Menu.t

(** Displays the menu on screen. *)
val display_menu : Tetravexview.Menu.t -> unit

(** Displays the current puzzle grid on screen. *)
val display_grid : unit -> unit

(** Displays the current set of tiles on screen. *)
val display_set : unit -> unit

(** [new_game m n] Opens up a new [n * m] tetravex puzzle to solve. *)
val new_game : int -> int -> unit

(** [open_from_file path] Opens up the tetravex 
  specified by the file located at [path]. *)
val open_from_file : string -> unit

(** Saves the current set of tiles at the given file path. *)
val save_as : string -> unit

(** Displays the solution of the current puzzle on the grid. *)
val solve : unit -> unit
