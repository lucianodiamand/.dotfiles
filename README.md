# dotfiles
My personal dot files

# Dependencies

## Nerd fonts

Download the fonts.
```wget -c https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/Hack.zip```

Uncompress in directory ```./local/share/fonts```

Run ```fc-cache -fv```

## lua-language-server

In debian I have to compile the sources. TODO

## typescript-language-server

To install:

```sudo npm i -g typescript-language-server```

## Packer

To install packer:
```git clone --depth 1 https://github.com/wbthomason/packer.nvim ~/.local/share/nvim/site/pack/packer/start/packer.nvim```

# Install the dotfiles

To install run:
```stow i3 nvim alacrity fish bin emacs```

