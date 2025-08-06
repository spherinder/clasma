{
  inputs.fenix = {
    url = "github:nix-community/fenix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { systems, nixpkgs, ... }@inputs:
    let eachSystem = f: nixpkgs.lib.genAttrs (import systems) (system: f {
          pkgs = nixpkgs.legacyPackages.${system};
          fenix = inputs.fenix.packages.${system};
        });
    in {
      devShells = eachSystem ({pkgs, fenix}: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (fenix.complete.withComponents [
              "cargo"
              "clippy"
              "rust-src"
              "rustc"
              "rustfmt"
              "miri"
            ])
            fenix.rust-analyzer
            cargo-expand
          ];
        };
      });
    };
}
