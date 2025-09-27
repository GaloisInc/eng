{
  description = "The eng engineering tool to support engineering work";

  nixConfig.bash-prompt-suffix = "eng.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      url = "github:kquick/nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fret-semantics = {
      url = "https://raw.githubusercontent.com/NASA-SW-VnV/fret/refs/heads/master/fret-electron/app/parser/semantics.json";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers, fret-semantics }:
    {
      apps = levers.eachSystem (s:
        rec {
          eng = {
            type = "app";
            program = "${self.packages.${s}.eng}/bin/eng";
          };
          default = eng;
        });
      devShells = levers.eachSystem (s:
        { default = self.packages."${s}".eng.env; });
      packages = levers.eachSystem (s:
        let pkgs = nixpkgs.legacyPackages.${s};
        in rec {
          default = eng;
          eng = pkgs.stdenv.mkDerivation {
            pname = "eng";
            version = "1.1";
            src = self;
            buildInputs = [ pkgs.curl pkgs.swi-prolog ];
            buildPhase = ''
              cp ${fret-semantics} src/semantics.json
              ${pkgs.bash}/bin/bash ./eng.sh dev build
              ./eng dev build
            '';
            installPhase = "mkdir -p $out/bin && cp ./eng $out/bin";
          };
        });
    };
}
