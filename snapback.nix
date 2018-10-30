{ mkDerivation, base, btrfs, directory, filepath, process, stdenv
, time, unix
}:
mkDerivation {
  pname = "snapback";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base btrfs directory filepath process time unix
  ];
  license = stdenv.lib.licenses.asl20;
}
