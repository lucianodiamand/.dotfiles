-- TODO maybe the definition of key leaders goes here

-- Indenting in visual mode (tab/shift+tab)
vim.keymap.set("v", "<Tab>", ">gv")
vim.keymap.set("v", "<S-Tab>", "<gv")

-- Remove search highlights after searching
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Remove search highlights" })

-- Exit from terminal mode
--vim.keymap.set("n", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- Disable arrow keys in normal mode
vim.keymap.set("n", "<left>", "<cmd>echo 'Use h to move!'<CR>")
vim.keymap.set("n", "<right>", "<cmd>echo 'Use l to move!'<CR>")
vim.keymap.set("n", "<up>", "<cmd>echo 'Use k to move!'<CR>")
vim.keymap.set("n", "<down>", "<cmd>echo 'Use j to move!'<CR>")

-- Makes vertical split
vim.keymap.set("n", "vv", "<C-W>v")
-- Makes horizontal split
vim.keymap.set("n", "bb", "<C-W>s")
-- Makes split windows equal width & height
vim.keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make splits equal size" })
-- Close current split window
vim.keymap.set("n", "<leader>sx", "<cmd>close<CR>", { desc = "Close current split" })

-- TODO can be replaced by christoomey/vim-tmux-navigator?
-- Quick jumping between splits
vim.keymap.set("n", "<C-j>", "<C-w>j")
vim.keymap.set("n", "<C-k>", "<C-w>k")
vim.keymap.set("n", "<C-h>", "<C-w>h")
vim.keymap.set("n", "<C-l>", "<C-w>l")

-- Keymaps for better default experience
--vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Move normally between wrapped lines
--vim.keymap.set("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
--vim.keymap.set("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Move to first symbol on the line
--vim.keymap.set("n", "H", "^")

-- Move to last symbol of the line
--vim.keymap.set("n", "L", "$")

-- Move to the end of yanked text after yank and paste
--vim.cmd("vnoremap <silent> y y`]")
--vim.cmd("vnoremap <silent> p p`]")
--vim.cmd("nnoremap <silent> p p`]")

-- Fixes pasting after visual selection
--vim.keymap.set("v", "p", '"_d')

