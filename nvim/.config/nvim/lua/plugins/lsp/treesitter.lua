return {
	"nvim-treesitter/nvim-treesitter",
	event = { "BufReadPost", "BufNewFile" },
	dependencies = {
		-- ts-autotag utilizes treesitter to understand the code structure to automatically close tsx tags
		"windwp/nvim-ts-autotag",
	},
	build = ":TSUpdate",
	enabled = true,
	config = function()
		require("nvim-treesitter.configs").setup({
			ensure_installed = {
				"vim",
				"vimdoc",
				"java",
				"javascript",
				"typescript",
				"lua",
				"ruby",
				"html",
				"css",
				"json",
				"tsx",
				"bash",
				"markdown",
				"markdown_inline",
				"gitignore",
				"xml",
				"c",
				"java",
			},
			indent = { enable = true },
			highlight = {
				enable = true,
				use_languagetree = true,
				-- disable = { "markdown" },
			},
			autotag = {
				enable = true,
			},
		})
	end,
	-- enable nvim-ts-context-commentstring plugin for commenting tsx and jsx
	require("ts_context_commentstring").setup({}),
}
