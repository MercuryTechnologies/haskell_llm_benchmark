{
  description = "Haskell dev environment with Stack and GHC";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.stack
            pkgs.ghc
            pkgs.cabal-install # optional, but recommended
            pkgs.zlib          # often needed by Haskell packages
            pkgs.python3
            pkgs.python3Packages.pip
            pkgs.python3Packages.setuptools
            pkgs.python3Packages.wheel
            # Additional libraries that might be needed for Python packages
            pkgs.pkg-config
            pkgs.libffi
            pkgs.gmp
            pkgs.ncurses
            pkgs.pandoc       # Required by pypandoc
            pkgs.portaudio    # Required by sounddevice
          ];

          shellHook = ''
            export STACK_ROOT=$PWD/.stack-root
            export PATH="$STACK_ROOT/bin:$PATH"
            
            # Python setup
            if [ ! -d "venv" ]; then
              python3 -m venv venv
            fi
            source venv/bin/activate
            
            # Install project dependencies
            echo "Installing Python dependencies..."
            pip install -e .[dev]
          '';
        };
      });
}
