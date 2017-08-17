printf '\033[0;34m%s\033[0m\n' "Upgrading all dotfiles"
cd "$DOTFILES/utils"
if git pull --rebase --stat origin master
then
  printf '\033[0;32m%s\033[0m\n' '       _       _    __ _ _            '
  printf '\033[0;32m%s\033[0m\n' '    __| | ___ | |_ / _(_) | ___  ___  '
  printf '\033[0;32m%s\033[0m\n' '   / _` |/ _ \| __| |_| | |/ _ \/ __| '
  printf '\033[0;32m%s\033[0m\n' '  | (_| | (_) | |_|  _| | |  __/\__ \ '
  printf '\033[0;32m%s\033[0m\n' '   \__,_|\___/ \__|_| |_|_|\___||___/ '
  printf '\033[0;32m%s\033[0m\n' ''
  printf '\033[0;34m%s\033[0m\n' 'Hooray! dotfiles have been updated and/or is at the current version.'
else
  printf '\033[0;31m%s\033[0m\n' 'There was an error updating. Try again later?'
fi








 

                            
                            
                            
                            
                            
