{ mkDerivation, attoparsec, base, shelly, stdenv, text, time }:
mkDerivation {
  pname = "workbalance";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ attoparsec base shelly text time ];
  description = "Shows a work balance";
  license = stdenv.lib.licenses.gpl3;
}
