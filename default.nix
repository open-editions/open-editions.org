let
  ribRevision = "a57e7ab7ef5b5b5fcbbf7b99b3edb33bfde5851f";
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
