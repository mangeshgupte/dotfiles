This repository tracks my custom dotfiles.

To update files in the submodule :
Suppose we change the file foo.
git add foo
git commit _m "message"
git push -u origin master

Then go the parent project
cd ..
git add submoduleProject
git commit -m "Update submodule"

To clone the repo on a new machine:
http://git-scm.com/book/en/Git-Tools-Submodules

git clone https://github.com/mangeshgupte/dotfiles
git submodule init
 # Fetch the data
git submodule update

To update files in the submodule
git merge
git submodule update

