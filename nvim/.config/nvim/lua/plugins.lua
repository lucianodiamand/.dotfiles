-- This file can be loaded by calling `lua require('plugins')` from your init.vim

local success, packer = pcall(require, 'packer')
if (not success) then
  print('Packer is not installed')
  return
end

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

packer.startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use {
    'lucianodiamand/neosolarized.nvim',
    --commit = '3dd9dda', -- Doesn't work
    --commit = '6848ba4', -- Work
    requires = { 'tjdevries/colorbuddy.nvim' }
  }

  -- Statusline
  use 'hoob3rt/lualine.nvim'

end)

