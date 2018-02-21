set -x 
rm -rf /tmp/breeze
mkdir -p /tmp/breeze
cp -Rsf $(readlink ./result)/* --no-preserve=mode /tmp/breeze/
cd /tmp/breeze
./bin/breeze-login -p 8080
