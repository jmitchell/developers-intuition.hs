{ mkDerivation, base, QuickCheck, stdenv }:
mkDerivation {
  pname = "devs-intuit";
  version = "0.1.0.0";
  src = ./..;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base QuickCheck ];
  description = "QuickCheck property-based testing demo";
  license = stdenv.lib.licenses.bsd3;
}
