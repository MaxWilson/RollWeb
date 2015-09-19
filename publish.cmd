git add . -A && git commit -m %1
git checkout gh-pages
git merge master
msbuild Roll.sln
REM git commit -a -m "Publish"
REM git push
REM git checkout master