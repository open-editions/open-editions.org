let
  ribRevision = "8e12871062f7f78e0b3ffd698c57d3d35f9f3e23";
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in { rib ? builtins.fetchTarball "https://github.com/JonathanReeve/rib/archive/${ribRevision}.tar.gz"
   , root ? gitignoreSource ./.  # Cabal project root
   , name ? "open-editions-org"  # Cabal project name
   , ...
   }:

import rib { inherit root name; }
