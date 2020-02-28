let
  ribRevision = "9ceff0f8c0189265f27fbe0dfaa2e3b7c09c8239";

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
