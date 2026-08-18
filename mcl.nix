# Build herumi/mcl to accelerate the bn254 / alt_bn128 precompiles (0x06 ecadd,
# 0x07 ecmul, 0x08 ecpairing).  mcl is NOT in nixpkgs and libff (which is) has
# no stable C ABI, so we build mcl from source; its C API (mclBn*, curve
# MCL_BN_SNARK1) is exactly the Ethereum bn254 curve.
#
# blst does NOT cover bn254 (it is BLS12-381 only), which is why this is a
# separate library from the one in shell.nix.
#
# Usage:
#   1. Review this expression (it fetches external cryptography source).
#   2. Fill in the correct `rev` and `hash` (run once with a fake hash; Nix
#      prints the real one), then:
#        nix-build mcl.nix -o .native-mcl
#      This produces .native-mcl/lib/libmclbn384_256.so.
#   3. Point LD_LIBRARY_PATH at it (or symlink into ./.native) and re-run;
#      native.rkt will pick up the bn254 accelerator once its FFI bindings are
#      wired against this library.
{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
  pname = "mcl";
  version = "1.99";
  src = pkgs.fetchFromGitHub {
    owner = "herumi";
    repo = "mcl";
    rev = "v1.99";                       # pin a reviewed tag
    hash = "sha256-hYFUXJ9mei0LFJVQIDblcdUu9/s8v6mZybIB/kbwxUI=";
  };
  # autoPatchelfHook rewrites the build-dir RPATHs to point at the real runtime
  # deps (sibling libmcl.so, gmp, libstdc++), so the C API lib loads standalone.
  nativeBuildInputs = [ pkgs.cmake pkgs.autoPatchelfHook ];
  buildInputs = [ pkgs.gmp pkgs.stdenv.cc.cc.lib ];
  # The 384_256 C API library covers both bn254 (SNARK1) and BLS12-381.
  cmakeFlags = [ "-DMCL_STATIC_LIB=OFF" ];
}
