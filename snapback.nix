{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "snapback";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.asl20;
}
