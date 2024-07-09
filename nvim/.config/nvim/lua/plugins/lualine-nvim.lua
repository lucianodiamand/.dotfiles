return {
  "nvim-lualine/lualine.nvim",
  config = function()
    local colors = {
      default_background = "#073642", -- base02
      default_text = "#93a1a1", -- base1
      modified_background = "#dc322f", -- red
      saved_background = "#859900", -- green
    }
    local theme = {
      normal = {
        a = { bg = colors.saved_background, fg = colors.default_text },
        b = { bg = colors.default_background, fg = colors.default_text },
        c = { fg = colors.default_text, bg = colors.default_background },
        z = { fg = colors.default_text, bg = colors.default_background },
      },
    }

    local function modified_text()
      if vim.bo.modified then
        return "✘"
      end
      return " "
    end

    require("lualine").setup({
      options = {
        theme = theme,
      },
      sections = {
        lualine_a = {
          {
            modified_text,
            separator = { right = "" },
            padding = {
              left = 3,
              right = 3,
            },
            color = function()
              if vim.bo.modified then
                return { bg = colors.modified_background, fg = colors.default_text }
              end
            end,
          },
        },
        lualine_b = {
          { "filename", file_status = false, path = 4 },
        },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
    })
  end,
}

