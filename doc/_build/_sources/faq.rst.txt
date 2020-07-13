==========================
Frequently Asked Questions
==========================

Why isn't this project compatible with ``ppxlib`` and other standard PPX rewriter infrastructure?
*************************************************************************************************

   One of the theses of this project, is that using Camlp5, and
   specifically Camlp5's AST, makes for faster and more-precise PPX
   rewriters.  Camlp5's AST is translated into Ocaml's AST as a final
   step, but for now there is no mechanism to go in the other
   direction.  In addition, even if there were, the PPX rewriting code
   would still manipulate Camlp5 AST framents, not Ocaml AST
   fragments.  Ocaml's AST has been designed to be efficient and
   commodious for type-checking and compilation; Camlp5's has been
   designed for efficient and programmer-friendly manipulation (for
   rewriting and macro-preprocessing generally).

.. container:: trailer
