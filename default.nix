{buildEnv, breeze-login, breeze-ui}:

buildEnv rec {
  name = "breeze-check-1.0.0";
  paths = [breeze-login breeze-ui];
}
