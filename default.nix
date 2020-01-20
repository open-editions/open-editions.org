let
  ribRevision = "ca3e9fbd1befbca868d3254559805eacba78e7eb";
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
