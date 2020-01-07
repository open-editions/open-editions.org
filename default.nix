let
  ribRevision = "c03c090d0f7af4869ea6a33d5af79b3496060abc";
in {
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/${ribRevision}.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "open-editions-org"
, ...
}:

import rib { inherit root name; }
