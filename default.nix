{ mkDerivation, base, Diff, QuickCheck, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "diff3";
  version = "0.2.0.3";
  src = ./.;
  buildDepends = [ base Diff ];
  testDepends = [
    base QuickCheck test-framework test-framework-quickcheck2
  ];
  homepage = "http://github.com/ocharles/diff3.git";
  description = "Perform a 3-way difference of documents";
  license = stdenv.lib.licenses.bsd3;
}
