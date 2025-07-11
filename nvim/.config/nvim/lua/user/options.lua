-- Remove ALL autocommands for the current group
vim.cmd("autocmd!")

-- Leader is 'Space' key
vim.g.mapleader = " "
-- Local leader is '\' key
vim.g.maplocalleader = "\\"

-- Encodings

-- Specify the character encoding used in the script
vim.scriptencoding = "uft-8"
-- Sets the character encoding used inside Vim
vim.opt.encoding = "utf-8"
-- Sets the character encoding for the file of this buffer
vim.opt.fileencoding = "utf-8"

-- Editor options

-- Print the line number in front of each line
vim.opt.number = true
-- Show the line number relative to the line with the cursor
vim.opt.relativenumber = true
-- Set width of line number column
--vim.opt.numberwidth = 2
-- Always show sign column
--vim.opt.signcolumn = "yes"
-- Show line at column 80
vim.opt.colorcolumn = "80"
-- Show invisible characters
vim.opt.list = true
-- When true, the title of the window will be set to value of 'titlestring'
vim.opt.title = true
-- Copy indent from current line when starting a new line
vim.opt.autoindent = true
-- Enable smart indentation
--vim.opt.smartindent = true
-- Search highlighting
vim.opt.hlsearch = true

-- Show (partial) command in the last line of the screen. Set this option off
-- if your terminal is slow
vim.opt.showcmd = true
-- TODO
--vim.opt.cmdheight = 0
-- The value of this option influences when the last window will have a status
-- line: 2 always
-- TODO what means 0
vim.opt.laststatus = 2
-- In Insert mode: use the appropiate number of spaces to insert a <Tab>
vim.opt.expandtab = true
-- Minimal number of screen lines to keep above and below the cursor
vim.opt.scrolloff = 10
-- Number of columns to keep to the left/right of cursor
--vim.opt.sidescrolloff = 8
-- TODO
--vim.opt.sidescroll = 5
-- Which terminal to use with :terminal or :!
vim.opt.shell = "zsh"
-- When nonempty, shows the effects of :substitute, :smagic, :snomagic and user
-- commands with the :command-preview flag as you type
vim.opt.inccommand = "split"
-- Ignore case when search for text
vim.opt.ignorecase = true
-- TODO
-- vim.opt.smartcase = true
-- When press tab-key, based on the existing indentation of the current line
vim.opt.smarttab = true
-- Enable smart indentation
--vim.opt.smartindent = 2
-- Whether vim adds extra identation to wrapped lines of text
vim.opt.breakindent = true
-- '>>' '<<' press set line
vim.opt.shiftwidth = 2
-- Controls tab the number of spaces
vim.opt.tabstop = 2
-- Number of spaces inserted for <Tab> key
--vim.opt.softtabstop = 2
-- Auto ident when press 'enter'
vim.opt.ai = true
-- Smart ident based space lever like '{}'
vim.opt.si = true
-- No wrap lines (display lines as single line)
vim.opt.wrap = false
-- Wrap lines at convenient points
--vim.opt.linebreak = true
-- Show line breaks
--vim.opt.showbreak = "<CR>"
-- If press 'backspace' when position beginning-line, move to end of previous
-- line, continue delete
-- TODO it can be: { "start", "eol", "indent" }
vim.opt.backspace = "start,eol,indent"
-- Finding files - Search down into subfolders
vim.opt.path:append({ "**" })
-- Ignore subfolders 'node_modules'
vim.opt.wildignore:append({ "*/node_modules/*" })
-- Force horizontal splits below current window
vim.opt.splitbelow = true
-- Force vertical splits right of current window
vim.opt.splitright = true
-- TODO
vim.opt.splitkeep = "cursor"
-- Enable mouse support: a = enabled
vim.opt.mouse = ""

-- TODO
--vim.opt.formatoptions:append({ 'r' })

-- Turn off paste mode wen leaving insert
-- nopaste is set pastemode off -> This will disable certain features like
-- autoindenting and mappings, which can prevent unintended side effects while
-- pasting
vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
	pattern = "*",
	command = "set nopaste",
})

-- This is a sequence of letters which describes how automatic formatting is to
-- be done
vim.opt.formatoptions:append({ "r" })

-- Backup files

-- No swap files
vim.opt.swapfile = false
-- Do not backup files
vim.opt.backup = false
-- TODO
vim.opt.writebackup = false
-- Skip backups when file is located under '/tmp'
vim.opt.backupskip = "/tmp/*"

-- Save undo history
vim.opt.undofile = true
vim.opt.undodir = os.getenv("HOME") .. "/.nvim/undodir"

-- General Behaviors
--vim.g.loadded_netrw = 1
--vim.g.loaded_netrwPlugin = 1

-- Highlight all matches in search
--vim.opt.cursorline = true

-- Decrease update time
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.opt.completeopt = "menuone,noselect"

-- NOTE: you should make sure your terminal supports this
vim.opt.termguicolors = true

-- Don't show modes (insert/visual)
vim.opt.showmode = false

-- Update vim after file update from outside
vim.opt.autoread = true

-- https://github.com/vim/vim/blob/master/runtime/doc/russian.txt
-- Enable hotkeys for Russian layout
-- vim.opt.langmap = ""
