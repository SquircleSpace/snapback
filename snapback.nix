{ mkDerivation, aeson, base, btrfs, bytestring, containers
, directory, filepath, process, stdenv, time, unix, yaml
}:
mkDerivation {
  pname = "snapback";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base btrfs bytestring containers directory filepath process
    time unix yaml
  ];
  license = stdenv.lib.licenses.asl20;
}
