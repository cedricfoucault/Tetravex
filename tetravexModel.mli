(** This module defines the models for Tetravex.
  
  It provides basic definitions for the logical data of a puzzle instance
  and operations over them.
*)

(** The model for the colors of the tiles' faces. *)
module Color : sig
  (** The type of a color. *)
  type t
  
  (** Returns the string label of a color *)
  val to_string : t -> string
  
  (** Returns the integer representation of a color. *)
  val to_int : t -> int
  
  (** Returns the color based on its integer representation. *)
  val of_int : int -> t
end

(** The model for Tetravex tiles. *)
module Tile : sig
  (** The type of a tile. *)
  type t
  
  (** Returns the empty tile. *)
  val empty : unit -> t
  
  (** Tells if a tile is empty *)
  val is_empty : t -> bool
  
  (** [Tile.make cr ct cl cb] returns a tile that has [cr] as its right color,
   [ct] as its top color, [cl] as its left color
   and [cb] as its bottom color. *)
  val make : Color.t -> Color.t -> Color.t -> Color.t -> t
  
  (** [Tile.copy t] returns a copy of t .*)
  val copy : t -> t
  
  (** Returns the tile's right color. *)
  val right_color : t -> Color.t
  
 (** Returns the tile's top color. *)
  val top_color : t -> Color.t
  
 (** Returns the tile's left color. *)
  val left_color : t -> Color.t
  
 (** Returns the tile's bottom color. *)
  val bottom_color : t -> Color.t  
end

(** The model for a Tetravex puzzle instance. *)
module Puzzle : sig
  (** The type of a Tetravex puzzle. *)
  type t
  
  (** [Puzzle.make m n c] returns a new puzzle made of a [m * n] grid,
  filled with empty tiles,
  where the tiles are made of at most [c] different colors. *)
  val make : int -> int -> int -> t
  
  (** [Puzzle.empty p] empties [p] (i.e fills it with empty tiles) *)
  val empty : t -> unit
  
  (** [Puzzle.copy p] returns a copy of [p]. *)
  val copy : t -> t
  
  (** [Puzzle.fill dest src] replaces each tile of [dest]
  with the corresponding tile of [src] *)
  val fill : t -> t -> unit
  
  (** Returns true if the given puzzle has no empty tile *)
  val is_full : t -> bool
  
  (** Returns the height [m] of the puzzle's grid (number of lines). *)
  val height : t -> int
  
  (** Returns the width [n] of te puzzle's grid (number of columns). *)
  val width : t -> int
  
  (** Returns the number of different colors that the tile faces can have. *)
  val colors : t -> int
  
  (** [Puzzle.get p i j] returns the tile located at position ([i], [j])
  on the [p]'s grid (indices start at 0). *)
  val get : t -> int -> int -> Tile.t
  
  (** [Puzzle.set p i j t] sets the tile of [p] indexed at [(i, j)] to be [t]. *)
  val set : t -> int -> int -> Tile.t -> unit
  
  (** [Puzzle.can_set p i j t] returns true if the tile [t]
  can be set in [p] grid at position [(i, j)]
  (i.e. the colors of the tile do match the existing adjacent tiles).*)
  val can_set : t -> int -> int -> Tile.t -> bool
  
  (** Returns the string representation of a puzzle instance. *)
  val to_string : t -> string
  
  (** Reads a puzzle instance from the named file. *)
  val read : string -> t
  
  (** Reads a puzzle instance from standard input. *)
  val read_in : unit -> t
  
  (** Saves a puzzle instance to the named file. *)
  val save : t -> string -> unit
  
  (** Writes a puzzle instance on standard output. *)
  val print : t -> unit
  
  (** [Puzzle.generate m n c] generates a random puzzle of size [m * n]
  {b which has a solution}
  [c] indicates the number of different colors for the tiles faces
  (if you don't know what to put, 10 is pretty cool :)). *)
  val generate : int -> int -> int -> t
  
  (** [Puzzle.dumb_generate m n c] generates a completely random puzzle
  of size [m * n] {b which doesn't necessarily have a solution} 
  [c] indicates the number of different colors for the tiles faces. *)
  val dumb_generate : int -> int -> int -> t
  
  (** The exception that is returned when no solution to a given puzzle
  was found. *)
  exception No_Solution
  
  (** Solves a Tetravex puzzle. *)
  val solve : t -> unit
  
  (** Returns true if the given puzzle is solved. *)
  val is_solved : t -> bool
  
  (** Returns a solved copy of the given puzzle. *)
  val get_solved : t -> t
end

(** @author "Cedric Foucault" *)
