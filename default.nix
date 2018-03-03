{buildEnv, breeze-login, breeze-ui}:

let
  breeze-log = breeze-login.overrideAttrs (oldAttrs: rec {
    installPhase = oldAttrs.installPhase + ''
      cd $out/snaplets/heist/templates/
      sed -i -e 's/elmVersion">.*<\/bind>/elmVersion">${breeze-ui.version}<\/bind>/' importElm.tpl
    '';
  });
in

buildEnv rec {
  name = "breeze-check-1.0.0";
  paths = [breeze-log breeze-ui];
}
