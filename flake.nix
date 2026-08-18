{
  description =
    "evm-redex — an executable PLT Redex/Racket specification of the EVM, with optional native crypto accelerators";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      # The native crypto story (libblst.so build, LD_LIBRARY_PATH) is Linux-only;
      # the pure Racket spec runs anywhere Racket does.
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f {
            inherit system;
            pkgs = import nixpkgs { inherit system; };
          });
    in {
      # ---- packages: the native crypto libraries, built reproducibly ----
      packages = forAllSystems ({ pkgs, ... }: {
        # herumi/mcl for bn254/alt_bn128 (not in nixpkgs); pinned in ./mcl.nix.
        mcl = import ./mcl.nix { inherit pkgs; };

        # nixpkgs ships blst as a static archive only; produce a shared object
        # so Racket's ffi-lib can dlopen it.  Fully self-contained (no deps).
        blst-shared =
          pkgs.runCommand "libblst-shared-${pkgs.blst.version}" {
            nativeBuildInputs = [ pkgs.stdenv.cc ];
          } ''
            mkdir -p $out/lib
            cc -shared -o $out/lib/libblst.so \
               -Wl,--whole-archive ${pkgs.blst}/lib/libblst.a -Wl,--no-whole-archive
          '';
      });

      # ---- devShell: everything needed to run the tests and reproduce the
      #      results (Racket+redex, and the native accelerators on the path) ----
      devShells = forAllSystems ({ system, pkgs }:
        let
          mcl = self.packages.${system}.mcl;
          blst-shared = self.packages.${system}.blst-shared;
          # gmp and secp256k1 come straight from nixpkgs; blst and mcl are built
          # above.  All four go on LD_LIBRARY_PATH so native.rkt picks them up.
          libPath = pkgs.lib.makeLibraryPath [
            pkgs.gmp
            pkgs.secp256k1
            blst-shared
            mcl
          ];
        in {
          default = pkgs.mkShell {
            # pkgs.racket is the full distribution and bundles redex-lib.
            # solc + foundry compile Solidity to the artifacts read by pbt/solc.rkt.
            packages = [ pkgs.racket pkgs.gmp pkgs.secp256k1 pkgs.solc pkgs.foundry ];
            shellHook = ''
              export LD_LIBRARY_PATH="${libPath}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              # the property-based-testing layer (evm-redex/pbt) needs rackcheck,
              # which is not in the base Racket distribution.
              if ! racket -e '(require rackcheck)' >/dev/null 2>&1; then
                echo "installing rackcheck (Racket package)..."
                raco pkg install --auto --scope user rackcheck >/dev/null 2>&1 || true
              fi
              # This shell's Racket has its own package scope, so link the checkout
              # into it -- that is what makes (require evm-redex) resolve here.
              if ! racket -e '(require evm-redex)' >/dev/null 2>&1; then
                echo "linking the evm-redex collection into this Racket..."
                raco pkg install --auto --scope user --name evm-redex --link "$PWD" \
                  >/dev/null 2>&1 || true
              fi
              echo "evm-redex dev shell ready."
              echo "  native accelerators on LD_LIBRARY_PATH: gmp, secp256k1, blst, mcl-bn254"
              echo "  solidity toolchain: solc, forge/cast/anvil (feed artifacts to evm-redex/pbt)"
              echo "  check the accelerators:"
              echo "    racket -e '(require evm-redex) native-status'"
              echo "  the test suites live in the companion evm-redex-tests project:"
              echo "    raco test unit/                 # unit suites"
              echo "    raco test pbt/                  # property-based testing DSL"
              echo "    raco test conformance/          # bundled conformance corpora"
              echo "    racket conformance/runner.rkt   # full parallel conformance (1274/1274)"
            '';
          };
        });

      # `nix fmt`
      formatter =
        forAllSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style or pkgs.nixpkgs-fmt);
    };
}
