return {
  "nvim-treesitter/nvim-treesitter",
  event = { "BufReadPost", "BufNewFile" },
  dependencies = {
    "windwp/nvim-ts-autotag",
  },
  enabled = true,
  config = function()
    require("nvim-treesitter.configs").setup({
      -- No intentará instalar ni compilar nada
      ensure_installed = {},

      indent = { enable = true },

      highlight = {
        enable = true,
        use_languagetree = true,
      },

      autotag = {
        enable = true,
      },
    })
  end,
  -- ya no se necesita build = ":TSUpdate",
}

