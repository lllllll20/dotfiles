require("packer").startup(function()
	use("wbthomason/packer.nvim")
	use("bluz71/vim-nightfly-guicolors")
--	use("vim-airline/vim-airline")
	use("nvim-lua/popup.nvim")
	use("nvim-lua/plenary.nvim")
	use("nvim-telescope/telescope.nvim")
  use("nvim-telescope/telescope-file-browser.nvim")
	use("neovim/nvim-lspconfig")
	use("williamboman/nvim-lsp-installer")
	use("kyazdani42/nvim-web-devicons")
	use("jose-elias-alvarez/null-ls.nvim")
	use({
		"numToStr/Comment.nvim",
		config = function()
			require("Comment").setup()
		end,
	})
  use ({
  'nvim-lualine/lualine.nvim',
  requires = { 'kyazdani42/nvim-web-devicons', opt = true }
})
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use "onsails/lspkind-nvim"
  use "mfussenegger/nvim-dap"
  use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"} }
  use "hrsh7th/nvim-cmp"
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-buffer"
  use "hrsh7th/cmp-path"
  use "hrsh7th/cmp-cmdline"
  -- use "hrsh7th/cmp-copilot"
  use "hrsh7th/cmp-nvim-lua"
  use "saadparwaiz1/cmp_luasnip"
  use "L3MON4D3/LuaSnip"
	use({ "windwp/nvim-autopairs" })
		use({
			"nvim-treesitter/nvim-treesitter",
			run = ":TSUpdate",
		})
end)


