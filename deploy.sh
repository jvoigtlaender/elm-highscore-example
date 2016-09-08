rm -rf out || exit 0;

elm-package install --yes
libsysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make --yes

mkdir out
libsysconfcpus/bin/sysconfcpus -n $CPUNUM elm-make Main.elm --output out/Main.html --yes
cd out

git init
git config user.name "Travis CI"
git config user.email "jvoigtlaender@users.noreply.github.com"
git add .
git commit -m "Travis deploy to gh-pages"
git push --force --quiet "https://$GH_TOKEN@github.com/$TRAVIS_REPO_SLUG" master:gh-pages >/dev/null 2>&1
