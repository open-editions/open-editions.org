let
  ribRevision = "a57e7ab7ef5b5b5fcbbf7b99b3edb33bfde5851f";

  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource;
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? gitignoreSource ./.
# Cabal project name
, name ? "open-editions-org"
, ...
}:

import rib { inherit root name; }
