Where am I?
===========

This is the root of the main development branch of MEDUSA.  The
directory that you can find here are 

  - DB/
  - DSP/
  - Main/
  - P2P
  - Player/

      They contain the sources for the main modules of MEDUSA.  One
      module, one directory.  Since each module can be implemented in
      different ways while remaining compatible with the others, each
      module directory has one subdirectory for every implementation.
      To know more about the module structure of MEDUSA, look in the
      doc subdirectory

  - lib/

      Software used by more than one module.

  - doc/

      Documents describing the overall structure of MEDUSA (but not
      the internal details of each module, those are described in doc/
      subdirectory of each module _implementation_)

