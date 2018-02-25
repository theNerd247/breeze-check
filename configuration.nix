{
  network.description = "mvc breeze checkin";
  network.enableRollback = true;

  breeze-check = {config, pkgs, ...}:
  let
    app = (import ./dev.nix { inherit pkgs; }).breeze-check;
  in
    {
      networking.firewall.allowedTCPPorts = [22 80];
      environment.systemPackages = [ app ];

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
          ./bin/breeze-login -p 80
        '';
      };
    };
}
