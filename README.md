This repository tracks my custom dotfiles. The technique for handling dotfiles is borrowed from
http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/. I used git submodules to handle
repositories like oh-my-zsh. The submodule commands are explained in http://git-scm.com/book/en/Git-Tools-Submodules.

 # Installing on a new machine

 ## To clone the repo on a new machine:

To clone the git repo and get all the submodules
```Shell
git clone https://github.com/mangeshgupte/dotfiles
git submodule init
git submodule update
```

Run the script to make symlinks to the dotfiles
```Shell
cd ~/dotfiles
./makesymlinks.sh
```

 ### Update the repo to get latest changes, including those in the submodules.
```Shell
git merge / git pull
git submodule update
```

 # Making changes in the local repo.

For files that are not in a submodule, the procedure is simple.
```Shell
git add foo
git commit _m "message"
git push -u origin master
```

If the file is within a submodule, then in addition, we need to do tell the parent project the following
```Shell
cd ..
git add submoduleProject
git commit -m "Update submodule"
```
