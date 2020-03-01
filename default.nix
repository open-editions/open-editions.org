let
  ribRevision = "e41eae3";
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in { rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
   , root ? gitignoreSource ./.  # Cabal project root
   , name ? "open-editions-org"  # Cabal project name
   , ...
   }:

import rib { inherit root name; }
