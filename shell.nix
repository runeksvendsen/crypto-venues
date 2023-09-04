(import ./default.nix).shellFor {
  tools = {
    cabal = "3.8.1.0";
  };
  withHoogle = false;
}
