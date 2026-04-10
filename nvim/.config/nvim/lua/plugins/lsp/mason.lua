return {
	{
		"williamboman/mason.nvim",
		dependencies = {
			"williamboman/mason-lspconfig.nvim",
			"WhoIsSethDaniel/mason-tool-installer.nvim",
		},
		config = function()
			local mason = require("mason")
			-- import mason-lspconfig
			local mason_lspconfig = require("mason-lspconfig")
			local mason_tool_installer = require("mason-tool-installer")

			mason.setup({})

			mason_lspconfig.setup({
				-- list of servers for mason to install
				ensure_installed = {
					"ts_ls",
					"html",
					"angularls",
					"cssls",
					--  "tailwindcss",
					--  "svelte",
					-- "lua_ls",
					--  "graphql",
					--  "emmet_ls",
					--  "prismals",
					--  "pyright",
					"jdtls",
					"clangd",
				},
				automatic_installation = true,
			})

			mason_tool_installer.setup({
				ensure_installed = {
					"prettier", -- prettier formatter
					"stylua", -- lua formatter
					-- "isort", -- python formatter
					-- "black", -- python formatter
					-- "pylint",
					"eslint_d",
					"prettierd",
				},
			})
		end,
	},
}
