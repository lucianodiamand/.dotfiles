-- This file can be loaded by calling `lua require('plugins')` from your init.vim

local success, packer = pcall(require, 'packer')
if (not success) then
  print('Packer is not installed')
  return
end

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return packer.startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

end)

