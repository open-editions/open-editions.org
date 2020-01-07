{
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/54e0b66.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "open-editions-org"
, ...
}:

import rib { inherit root name; }
