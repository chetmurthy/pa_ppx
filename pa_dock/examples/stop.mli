class type foo =
   object
     (** comment for method m *)
     method m : string

     (**/**)

     (** This method won't appear in the documentation *)
     method bar : int
   end

 (** This value appears in the documentation, since the Stop special comment
     in the class does not affect the parent module of the class.*)
 val foo : string

 (**/**)
 (** The value bar does not appear in the documentation.*)
 val bar : string
 (**/**)

 (** The type t appears since in the documentation since the previous stop comment
 toggled off the "no documentation mode". *)
type t = string
