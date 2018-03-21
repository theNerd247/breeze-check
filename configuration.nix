{
  network.description = "mvc breeze checkin";
  network.enableRollback = true;

  breeze-check = {config, pkgs, ...}:
  let
    app = (import ./dev.nix { inherit pkgs; }).breeze-check;
    cert = pkgs.callPackage ./cert.nix {};


    certPath="${cert}/${cert.certPath}";
  in
    {
      networking.firewall.allowedTCPPorts = [22 443 ];
      environment.systemPackages = [ app cert ];

      systemd.services.breeze-check = {
        description = "the MVC Breeze Check-in app";
        wantedBy = ["multi-user.target"];
        after = ["network.target"];
        script = ''
          set -x 
          rm -rf /tmp/breeze
          mkdir -p /tmp/breeze
          cp -Rsf ${app}/* --no-preserve=mode /tmp/breeze/
          cd /tmp/breeze
          ./bin/breeze-login
            -p 443\
            --ssl-port=443\
            --no-access-log
            --ssl-cert=${certPath}/fulllchain.pem\
            --ssl-cert=${certPath}/privkey.pem\
            --ssl-key-p 443
        '';
      };
    };
}
