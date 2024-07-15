return {
	-- the colorscheme should be available when starting Neovim
	"maxmx03/solarized.nvim",
	-- make sure we load this during startup if it is your main colorscheme
	lazy = false,
	-- make sure to load this before all the other start plugins
	priority = 1000,
	config = function()
		vim.o.background = "dark" -- or 'light'
		-- load the colorscheme here
		vim.cmd.colorscheme("solarized")
	end,
}
