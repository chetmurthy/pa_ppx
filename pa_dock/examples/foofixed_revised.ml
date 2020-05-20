(** The first special comment of the file is the comment associated
     to the whole module. *)

 (** The comment for function f *)
 value f x y = x + y ;

 (** This comment is not attached to any element since there is another
     special comment just before the next element. *)

 (** Comment for exception My_exception, even with a simple comment
     between the special comment and the exception.*)
 (* A simple comment. *)
 exception My_exception of (int -> int) and int ;

 (** Comment for type weather  *)
 type weather = [
   Rain of int (** The comment for constructor Rain *)
 | Sun (** The comment for constructor Sun *)
 ] ;

 (** The comment for type my_record *)
 type my_record = {
     foo : int ;    (** Comment for field foo *)
     bar : string (** Comment for field bar *)
   } ;

  class cl = object end ;

 (** The comment for class my_class *)
 class my_class =
     object
       (** A comment to describe inheritance from cl *)
       inherit cl ;

       (** The comment for the instance variable tutu *)
       value mutable tutu = "tutu" ;

       (** The comment for toto *)
       value toto = 1 ;
       value titi = "titi" ;

       (** Comment for method toto *)
       method toto = tutu ^ "!" ;

       (** Comment for method m *)
       method m (f : float) = 1 ;
     end ;

 (** The comment for class type my_class_type *)
 class type my_class_type =
   object
     (** The comment for the instance variable x. *)
     value mutable x : int ;

     (** The comment for method m. *)
     method m : int -> int ;
   end ;

 (** The comment for module Foo *)
 module Foo =
   struct
     (** The comment for x *)
     value x = 0 ;

     (** A special comment in the class, but not associated to any element. *)
   end ;

 (** The comment for module type my_module_type. *)
 module type my_module_type =
   sig
     (** Comment for value x. *)
     value x : int ;
     (* ... *)
   end ;

(** a special comment at the end of the toplevel module *)
