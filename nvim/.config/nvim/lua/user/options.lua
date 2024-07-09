
-- Remove ALL autocommands for the current group
vim.cmd('autocmd!')

-- Encodings

-- Specify the character encoding used in the script
vim.scriptencoding = 'uft-8'
-- Sets the character encoding used inside Vim
vim.opt.encoding = 'utf-8'
-- Sets the character encoding for the file of this buffer
vim.opt.fileencoding = 'utf-8'

-- Editor options

-- Print the line number in front of each line
vim.wo.number = true
-- Show the line number relative to the line with the cursor
vim.opt.relativenumber = true
-- Show line at column 80
vim.opt.colorcolumn = '80'
-- Show invisible characters
vim.opt.list = true
-- When true, the title of the window will be set to value of 'titlestring'
vim.opt.title = true
-- Copy indent from current line when starting a new line
vim.opt.autoindent = true
-- Search highlighting
vim.opt.hlsearch = true

-- Show (partial) command in the last line of the screen. Set this option off
-- if your terminal is slow
vim.opt.showcmd = true
-- The value of this option influences when the last window will have a status
-- line: 2 always
vim.opt.laststatus = 2
-- In Insert mode: use the appropiate number of spaces to insert a <Tab>
vim.opt.expandtab = true
-- Minimal number of screen lines to keep above and below the cursor
vim.opt.scrolloff = 10
-- Which terminal to use with :terminal or :!
vim.opt.shell = 'fish'
-- When nonempty, shows the effects of :substitute, :smagic, :snomagic and user
-- commands with the :command-preview flag as you type
vim.opt.inccommand = 'split'
-- Ignore case when search for text
vim.opt.ignorecase = true
-- When press tab-key, based on the existing indentation of the current line
vim.opt.smarttab = true
-- Whether vim adds extra identation to wrapped lines of text
vim.opt.breakindent = true
-- '>>' '<<' press set line
vim.opt.shiftwidth = 2
-- Controls tab the number of spaces
vim.opt.tabstop = 2
-- Auto ident when press 'enter'
vim.opt.ai = true
-- Smart ident based space lever like '{}'
vim.opt.si = true
-- No wrap lines
vim.opt.wrap = false
-- If press 'backspace' when position beginning-line, move to end of previous
-- line, continue delete
vim.opt.backspace = 'start,eol,indent'
-- Finding files - Search down into subfolders
vim.opt.path:append { '**' }
-- Ignore subfolders 'node_modules' 
vim.opt.wildignore:append { '*/node_modules/*' }

-- Turn off paste mode when leaving insert
-- nopaste is set pastemode off -> This will disable certain features like
-- autoindenting and mappings, which can prevent unintended side effects while
-- pasting
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = '*',
  command = "set nopaste"
})

-- This is a sequence of letters which describes how automatic formatting is to
-- be done
vim.opt.formatoptions:append { 'r' }

-- Backup files
-- Do not backup files
vim.opt.backup = false
-- Skip backups when file is located under '/tmp'
vim.opt.backupskip = '/tmp/*'

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

