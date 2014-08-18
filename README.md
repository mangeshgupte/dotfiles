This repository tracks my custom dotfiles. The technique for handling dotfiles is borrowed from
http://blog.smalleycreative.com/tutorials/using-git-and-github-to-manage-your-dotfiles/. I used git submodules to handle
repositories like oh-my-zsh. The submodule commands are explained in http://git-scm.com/book/en/Git-Tools-Submodules.

# Installing on a new machine

## To clone the repo on a new machine:

To clone the git repo and get all the submodules
```Shell
git clone https://github.com/mangeshgupte/dotfiles
cd dotfiles
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

# Syncing a fork

From time to time, you will need to update the submodules present in your
repository. Github has simple instructions on doing this at https://help.github.com/articles/configuring-a-remote-for-a-fork
First (once) you need to configure a remote for the fork.
```Shell
git remote -v
git remote add upstream https://github.com/ORIGINAL_OWNER/ORIGINAL_REPOSITORY.git
```

Then, you can actually sync the fork. Github instructions at https://help.github.com/articles/syncing-a-fork
```Shell
git fetch upstream
git checkout master
git merge upstream/master
```

Don't forget to push changes and update the submodule after doing this.
