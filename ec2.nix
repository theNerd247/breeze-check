let
  region = "us-east-2";
  accessKeyId = "breeze";

in
{ breeze-check =
  { resources, ... }:
  { deployment.targetEnv = "ec2";
    deployment.ec2.accessKeyId = accessKeyId;
    deployment.ec2.region = region;
    deployment.ec2.instanceType = "t2.micro";
    deployment.ec2.keyPair = resources.ec2KeyPairs.breeze-check-keys;

  };

  resources.ec2KeyPairs.breeze-check-keys =
    { inherit region accessKeyId; };
  }
