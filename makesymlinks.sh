#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

########## Variables

dir=~/dotfiles                    # dotfiles directory
olddir=~/.dotfiles.old             # old dotfiles backup directory
files="zshrc oh-my-zsh aliases.sh emacs emacs.d my.cnf grcat tmux.conf"    # list of files/folders to symlink in homedir

##########

# create dotfiles_old in homedir
echo -n "Creating $olddir for backup of any existing dotfiles in ~ ... "
mkdir -p $olddir
echo "done"

# change to the dotfiles directory
echo -n "Changing to the $dir ... "
cd $dir
echo "done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
echo "Backing up and linking files"
for file in $files; do
	echo -n "$file ... "
	mv ~/.$file $olddir
	echo -n "moved ... "
	ln -s $dir/$file ~/.$file
	echo "linked"
done

echo "Creating empty files"
touch ~/.z

echo "Done"
