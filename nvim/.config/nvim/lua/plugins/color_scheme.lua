return {
  -- the colorscheme should be available when starting Neovim
  {
    "maxmx03/solarized.nvim",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000, -- make sure to load this before all the other start plugins
    config = function()
      vim.o.background = 'dark' -- or 'light'
      -- load the colorscheme here
      vim.cmd.colorscheme 'solarized'
    end,
  }
}

