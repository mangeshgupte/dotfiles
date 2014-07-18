This repository tracks my custom dotfiles.

To update files in the submodule :
Suppose we change the file foo.
```Shell
git add foo
git commit _m "message"
git push -u origin master
```

Then go the parent project
```Shell
cd ..
git add submoduleProject
git commit -m "Update submodule"
```
To clone the repo on a new machine:
http://git-scm.com/book/en/Git-Tools-Submodules

```Shell
git clone https://github.com/mangeshgupte/dotfiles
git submodule init
 # Fetch the data
git submodule update
```

To update files in the submodule
```Shell
git merge / git pull
git submodule update
```
