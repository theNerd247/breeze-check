{pkgs, fetchgit}:

let
  config = builtins.fromJSON (builtins.readFile ./simple.json);

  src = fetchgit {
    inherit (config) 
      url 
      rev
      sha256;
  };
in

{
    inherit (import "${src}/simple-core/dev.nix") simple-core;
    inherit (import "${src}/simple-aeson/dev.nix") simple-aeson;
    inherit (import "${src}/simple-snap/dev.nix") simple-snap;
    inherit (import "${src}/simple-string/dev.nix") simple-string;
}
