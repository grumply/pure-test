{ mkDerivation, base, deepseq, directory, unordered-containers, hashable, mtl, stm, async, pure-random-pcg, pure-spacetime, stdenv }:
mkDerivation {
  pname = "pure-test";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq directory unordered-containers hashable mtl stm async pure-random-pcg pure-spacetime ];
  homepage = "github.com/grumply/pure-test";
  license = stdenv.lib.licenses.bsd3;
}