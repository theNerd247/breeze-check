{buildEnv, breeze-login, breeze-ui}:

let
  breeze-log = breeze-login.overrideAttrs (oldAttrs: rec {
    installPhase = oldAttrs.installPhase + ''
      cd $out/snaplets/heist/templates/
      sed -i -e 's/elmVersion">.*<\/bind>/elmVersion">${breeze-ui.version}<\/bind>/' importElm.tpl
      sed -i -e 's/elmVersion">.*<\/bind>/elmVersion">${breeze-ui.version}<\/bind>/' importAdminElm.tpl
    '';
  });
in

buildEnv rec {
  name = "breeze-check-1.5.0";
  paths = [breeze-log breeze-ui];
}
