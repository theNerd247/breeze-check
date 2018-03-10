# breeze-login

## Dev

### Nix

If you have [nix](https://nixos.org/nix/manual/) then you don't need to install
anything just run:

    nix-shell

from the root of the project. This will download and install all dependencies
and place you in a bash shell that has its PATH modified so you can build the
app.

To build and run the server:

    cd breeze-login/
    cabal run breeze-login

To build the elm UI run 

    cd ui/
    elm make --output elm.js UI.elm

## Build

### Nix

    nix-build -A breeze-check dev.nix

Run the web app locally run the following from the root of the project:

    ./run.sh
