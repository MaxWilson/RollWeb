git add . -A && git commit -m %1
git checkout gh-pages
git merge master
msbuild Roll.sln
git commit -a -m "Publish script" && git push
git checkout master
