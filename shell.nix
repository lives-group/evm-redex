# Development shell that enables the native crypto accelerators (private/native.rkt).
#
#   nix-shell            # builds libblst.so, puts gmp/secp256k1/blst on the path
#   racket -e '(require evm-redex) native-status'              # => all 'native
#
# blst ships as a static archive in nixpkgs, so we build a shared object from it
# on first entry (into ./.native).  gmp and secp256k1 already provide .so files.
# When you are NOT in this shell, native.rkt falls back to the pure Racket code
# (byte-identical), so nothing here is required to run the specification.
{ pkgs ? import <nixpkgs> {} }:
let
  mcl = import ./mcl.nix { inherit pkgs; };   # bn254 (alt_bn128) via herumi/mcl
in
pkgs.mkShell {
  # solc + foundry compile Solidity to the artifacts read by pbt/solc.rkt.
  buildInputs =
    [ pkgs.racket pkgs.gmp pkgs.secp256k1 pkgs.blst pkgs.stdenv.cc mcl
      pkgs.solc pkgs.foundry ];
  shellHook = ''
    mkdir -p .native
    if [ ! -f .native/libblst.so ] || [ ${pkgs.blst}/lib/libblst.a -nt .native/libblst.so ]; then
      echo "building .native/libblst.so from ${pkgs.blst}/lib/libblst.a"
      cc -shared -o .native/libblst.so \
         -Wl,--whole-archive ${pkgs.blst}/lib/libblst.a -Wl,--no-whole-archive
    fi
    export LD_LIBRARY_PATH="$PWD/.native:${pkgs.lib.makeLibraryPath [ pkgs.gmp pkgs.secp256k1 mcl ]}:$LD_LIBRARY_PATH"
    # the property-based-testing layer (evm-redex/pbt) needs rackcheck.
    if ! racket -e '(require rackcheck)' >/dev/null 2>&1; then
      echo "installing rackcheck (Racket package)..."
      raco pkg install --auto --scope user rackcheck >/dev/null 2>&1 || true
    fi
    # link the checkout into this Racket's package scope, so (require evm-redex) resolves.
    if ! racket -e '(require evm-redex)' >/dev/null 2>&1; then
      echo "linking the evm-redex collection into this Racket..."
      raco pkg install --auto --scope user --name evm-redex --link "$PWD" >/dev/null 2>&1 || true
    fi
    echo "native accelerators ready (blst + secp256k1 + gmp + mcl-bn254); solc + foundry on PATH"
  '';
}
