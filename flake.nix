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

        # fastuuid - new dependency for litellm >= 1.80 (Rust binary wheel)
        fastuuid = pkgs.python3Packages.buildPythonPackage rec {
          pname = "fastuuid";
          version = "0.14.0";
          format = "wheel";
          src = pkgs.fetchurl ({
            "aarch64-darwin" = {
              url = "https://files.pythonhosted.org/packages/54/ea/682551030f8c4fa9a769d9825570ad28c0c71e30cf34020b85c1f7ee7382/fastuuid-0.14.0-cp312-cp312-macosx_11_0_arm64.whl";
              hash = "sha256-0j7wb55nFjvjjOznBBcEhnFbF39rquM4EQmD+ZpywHA=";
            };
            "x86_64-darwin" = {
              url = "https://files.pythonhosted.org/packages/2b/b3/c846f933f22f581f558ee63f81f29fa924acd971ce903dab1a9b6701816e/fastuuid-0.14.0-cp312-cp312-macosx_10_12_x86_64.whl";
              hash = "sha256-yqHxTSECy401MJa8bvbBOyyB80fmq51vvUi53qQcFT0=";
            };
            "x86_64-linux" = {
              url = "https://files.pythonhosted.org/packages/16/6e/c0fb547eef61293153348f12e0f75a06abb322664b34a1573a7760501336/fastuuid-0.14.0-cp312-cp312-manylinux_2_17_x86_64.manylinux2014_x86_64.whl";
              hash = "sha256-gIUn8kB/WKdskW1qoV1YaSpKAZ/fjUwyrH/zA7fXrwk=";
            };
            "aarch64-linux" = {
              url = "https://files.pythonhosted.org/packages/14/dd/5927f0a523d8e6a76b70968e6004966ee7df30322f5fc9b6cdfb0276646a/fastuuid-0.14.0-cp312-cp312-manylinux_2_17_aarch64.manylinux2014_aarch64.whl";
              hash = "sha256-DJ7GBaziQ7bb470n691dM7ANjR0/WAs5/dFc2W/XF5Y=";
            };
          }.${system});
          doCheck = false;
        };

        # Override litellm to a newer version that has BadGatewayError etc.
        litellm-new = pkgs.python3Packages.litellm.overridePythonAttrs (old: rec {
          version = "1.81.10";
          src = pkgs.python3Packages.fetchPypi {
            pname = "litellm";
            inherit version;
            hash = "sha256-jXaacgCIjhKVWSr1zlyw/wNYMiUL0BAqTKUKz1ggylA=";
          };
          dependencies = (old.dependencies or []) ++ [ fastuuid ];
          doCheck = false;
          dontCheckRuntimeDeps = true;
          pythonImportsCheck = [ "litellm" ];
          # Remove broken enterprise symlink
          postFixup = (old.postFixup or "") + ''
            find $out -xtype l -name "litellm_enterprise" -delete || true
          '';
        });

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
          litellm-new
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
          oslex

          # Additional dependencies for visualization
          imgcat
          lox
          black
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
            pkgs.pre-commit
          ];

          shellHook = ''
            export STACK_ROOT=$PWD/.stack-root
            export PATH="$STACK_ROOT/bin:$PATH"

            # Enable matplotlib to find the right backend
            export PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}:$PYTHONPATH
            export MATPLOTLIB_BACKEND=Agg

            # Install pre-commit hooks if not already installed
            if [ ! -f .git/hooks/pre-commit ] || ! grep -q "pre-commit" .git/hooks/pre-commit 2>/dev/null; then
              pre-commit install --allow-missing-config
            fi
            
            echo "Haskell LLM benchmark environment ready"
            echo "Python packages pre-installed in the environment"
          '';
        };
      });
}
