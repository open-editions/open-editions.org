{
  description = "Open-editions.org website.";
  inputs = {
    nixpkgs.url = "github:nixpkgs/nixpkgs/22dc22f8cedc58fcb11afe1acb08e9999e78be9c";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    # rib.url = "/home/jon/Code/rib";
    rib.url = "github:JonathanReeve/rib";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, rib, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        name = "open-editions.org";
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; config.allowBroken = true; };
        # Based on https://github.com/input-output-hk/daedalus/blob/develop/yarn2nix.nix#L58-L71
        filter = name: type:
          let
            baseName = baseNameOf (toString name);
            sansPrefix = pkgs.lib.removePrefix (toString ./.) name;
          in
          # Ignore these files when building source package
            !(
              baseName == "README.md" ||
              sansPrefix == "/bin" ||
              sansPrefix == "/content" ||
              sansPrefix == "/.github" ||
              sansPrefix == "/.vscode" ||
              sansPrefix == "/.ghcid"
            );
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv name;
            root = pkgs.lib.cleanSourceWith { inherit filter name; src = ./.; };
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # ema = disableCabalFlag inputs.ema.defaultPackage.${system} "with-examples";
              # lvar = self.callCabal2nix "lvar" inputs.ema.inputs.lvar { }; # Until lvar gets into nixpkgs
              rib = inputs.rib.defaultPackage.${system};
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
                pkgs.nixpkgs-fmt
              ]);
          };
      in
      {
        # Used by `nix build` & `nix run`
        defaultPackage = project false;

        # Used by `nix develop`
        devShell = project true;
      });
}
