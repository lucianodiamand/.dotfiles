-- use space as leader key
vim.g.mapleader = ' '

-- open netrw with <leader>pv keys
-- TODO Use on version > 0.8
--vim.keymap.set('n', '<leader>pv', vim.cmd.Ex)
vim.keymap.set('n', '<leader>pv', ':Ex<CR>')

