rm -r ./js
mkdir js

# fetch the app version from the ui default.nix
v=$(grep "version.\?=" ../ui/default.nix | sed -e 's/.*"\(.*\)";/\1/')
echo "running ui version: $v"
ln -s ../ui/elm.js ./js/elm.$v.js

# replace the version number in the template file so our web app points to the
# right place
cmd="s/elmVersion\">.*<\\/bind>/elmVersion\">$v<\\/bind>/"
sed -i -e "$cmd" ./snaplets/heist/templates/importElm.tpl

cabal run breeze-login -- -p 8080
