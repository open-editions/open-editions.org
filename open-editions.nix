{ mkDerivation, base, blaze-html, clay, hakyll, hspec, stdenv }:
mkDerivation {
  pname = "open-editions";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html clay hakyll hspec ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
