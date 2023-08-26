
-- Highlight the screen line of the cursor with CursorLine
vim.opt.cursorline = true
-- True color is wide color of standard 257 color in vim
vim.opt.termguicolors = true
-- This option controls the transparency of floating windows
vim.opt.winblend = 0
-- When we type files names or directory paths in vim command-line
-- p: show the preview window when multiple matches are found
-- u: auto-insert characters that can complete the match
-- m: show matches in a pop-up menu
vim.opt.wildoptions = 'pum'
-- Set transparecy popup
vim.opt.pumblend = 5
vim.opt.background = 'dark'

