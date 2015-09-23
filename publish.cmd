git checkout gh-pages
git merge master
msbuild Roll.sln
<<<<<<< HEAD
git commit -a -m "Publish script" && git push
=======
git commit -a -m "Update JavaScript from master branch"
git push
>>>>>>> master
git checkout master
