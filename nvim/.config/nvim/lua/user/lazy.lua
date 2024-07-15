-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
  spec = {
    -- import your plugins
    { import = "plugins" },
    { import = "plugins.lsp" },
  },
  -- Configure any other settings here. See the documentation for mode details.
  -- colorscheme that will be used when installing plugins.
  install = { colorscheme = { "solarized" } },
  -- automatically check for plugin updates
  checker = {
    -- Automatically check for package updates
    enable = false,
    -- Don't spam us with notification every time there is an update available
    notify = false,
  },
  change_detection = {
    -- Don't notify us every time a change is made to the configuration
    notify = false,
  },
})
