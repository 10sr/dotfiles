~/.bash_profile           <= console login
|
|--~/.dotfiles/profile
|
`--~/.bashrc
   |
   `--~/.dotfiles/bashrc

~/.xinitrc                <= startx (~/.config/openbox/autostart also executed)
|
`--openbox-session
   |
   |--~/.dotfiles/xprofile
   |
   `--~/.dotfiles/xprograms

