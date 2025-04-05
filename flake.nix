{
  description = "Haskell + Python env for benchmarking";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Python with required packages
        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          # Core benchmark analysis dependencies
          pandas
          matplotlib
          numpy
          
          # Additional dependencies needed for the benchmark
          pip
          setuptools
          wheel
          backoff
          beautifulsoup4
          configargparse
          diff-match-patch
          diskcache
          flake8
          gitpython
          grep-ast
          importlib-metadata
          importlib-resources
          json5
          jsonschema
          litellm
          mixpanel
          networkx
          packaging
          pathspec
          pexpect
          pillow
          posthog
          prompt-toolkit
          psutil
          pydub
          pypandoc
          pyperclip
          pyyaml
          rich
          scipy
          socksio
          sounddevice
          soundfile
          typer
          watchfiles
          
          # Additional dependencies for visualization
          imgcat
          lox
        ]);
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.stack
            pkgs.ghc
            pkgs.cabal-install # optional, but recommended
            pkgs.zlib          # often needed by Haskell packages
            pkgs.direnv
            pythonEnv          # Python with pre-installed packages
            # Additional libraries that might be needed
            pkgs.pkg-config
            pkgs.libffi
            pkgs.gmp
            pkgs.ncurses
            pkgs.pandoc       # Required by pypandoc
            pkgs.portaudio    # Required by sounddevice
            pkgs.gcc
            pkgs.gfortran
          ];

          shellHook = ''
            export STACK_ROOT=$PWD/.stack-root
            export PATH="$STACK_ROOT/bin:$PATH"
            
            # Enable matplotlib to find the right backend
            export PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}:$PYTHONPATH
            export MATPLOTLIB_BACKEND=Agg

            echo "Haskell LLM benchmark environment ready"
            echo "Python packages pre-installed in the environment"
          '';
        };
      });
}
