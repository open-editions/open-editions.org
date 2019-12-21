{
# Rib library source to use
  rib ? builtins.fetchTarball "https://github.com/srid/rib/archive/c43b422.tar.gz"
# Cabal project root
, root ? ./.
# Cabal project name
, name ? "open-editions-org"
, ...
}:

import rib { inherit root name; }
