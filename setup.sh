ln -fs ~/dotfiles/.vimrc ~/.vimrc
ln -fs ~/dotfiles/.vimrc.bundles ~/.vimrc.bundles
ln -fs ~/dotfiles/.zshrc ~/.zshrc
ln -fs ~/dotfiles/.tmux.conf ~/.tmux.conf
# needed for tmux
brew install reattach-to-user-namespace
# add ag global ignore pattern - needed for now as ag does not respect git ignore properly yet
ln -fs ~/dotfiles/.agignore ~/.agignore
