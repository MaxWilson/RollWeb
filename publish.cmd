git checkout gh-pages
git merge master
msbuild Roll.sln
git commit -a -m "Update JavaScript from master branch"
git push
git checkout master
