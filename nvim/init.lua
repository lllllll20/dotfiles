require("packerconfig")
-- require("packer").startup(function()
-- 	use("wbthomason/packer.nvim")
-- 	use("bluz71/vim-nightfly-guicolors")
-- --	use("vim-airline/vim-airline")
-- 	use("nvim-lua/popup.nvim")
-- 	use("nvim-lua/plenary.nvim")
-- 	use("nvim-telescope/telescope.nvim")
--   use("nvim-telescope/telescope-file-browser.nvim")
-- 	use("neovim/nvim-lspconfig")
-- 	use("williamboman/nvim-lsp-installer")
-- 	use("L3MON4D3/LuaSnip")
-- 	use("kyazdani42/nvim-web-devicons")
-- 	use("jose-elias-alvarez/null-ls.nvim")
-- 	use({
-- 		"numToStr/Comment.nvim",
-- 		config = function()
-- 			require("Comment").setup()
-- 		end,
-- 	})
--   use ({
--   'nvim-lualine/lualine.nvim',
--   requires = { 'kyazdani42/nvim-web-devicons', opt = true }
-- })
-- 	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
-- 	use({ "windwp/nvim-autopairs" })
-- 	use({ "saadparwaiz1/cmp_luasnip" })
-- 	use({ "hrsh7th/cmp-nvim-lsp" })
-- 	use({ "hrsh7th/cmp-path" })
-- 	use({
-- 		"hrsh7th/nvim-cmp",
-- 		requires = {
-- 			"hrsh7th/cmp-buffer",
-- 		},
-- 		use({
-- 			"nvim-treesitter/nvim-treesitter",
-- 			run = ":TSUpdate",
-- 		}),
-- 	})
-- end)

-- ENVS
HOME = os.getenv("HOME")

-- lualine

require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'nightfly',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = false,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}


--Treesitter

require("nvim-treesitter.configs").setup({
	ensure_installed = { "bash", "lua", "python" },
	highlight = {
		enable = true,
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "<CR>",
			scope_incremental = "<CR>",
			node_incremental = "<TAB>",
			node_decremental = "<S-TAB>",
		},
	},
})

-- Setting up the language servers and their keybindings
--require'lspconfig'.pyright.setup{}
--require'lspconfig'.sumneko_lua.setup{}

local nvim_lsp = require("lspconfig")

local on_attach = function(client, bufnr)
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end
	local function buf_set_option(...)
		vim.api.nvim_buf_set_option(bufnr, ...)
	end

	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Mappings.
	local opts = { noremap = true, silent = true }
	buf_set_keymap("n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
	buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
	buf_set_keymap("n", "<A-r>", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
	buf_set_keymap("n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
	buf_set_keymap("n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
	buf_set_keymap("n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
	buf_set_keymap("n", "<leader>t", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local servers = { "pyright", "bashls" }
for _, lsp in ipairs(servers) do
	nvim_lsp[lsp].setup({
		on_attach = on_attach,
		flags = {
			debounce_text_changes = 150,
		},
	})
end

--lua-language-server setup
-- Make runtime files discoverable to the server
local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")
require("lspconfig").sumneko_lua.setup({
	cmd = { "lua-language-server" },
	on_attach = on_attach,
	--capabilities = capabilities,
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
				-- Setup your lua path
				path = runtime_path,
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
})

-- null ls setup

require("null-ls").setup({
	sources = {
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.prettier.with({ filetypes = { "html", "json", "yaml", "markdown" } }),
		require("null-ls").builtins.diagnostics.shellcheck,
		require("null-ls").builtins.formatting.black,
		require("null-ls").builtins.formatting.shfmt,
	},
})

require('nvim-autopairs').setup{}


--vim.o.completeopt = "menuone,noselect"

local lsp_symbols = {

	Text = "   (Text) ",
	Method = "   (Method)",
	Function = "   (Function)",
	Constructor = "   (Constructor)",
	Field = " ﴲ  (Field)",
	Variable = "[] (Variable)",
	Class = "   (Class)",
	Interface = " ﰮ  (Interface)",
	Module = "   (Module)",
	Property = " 襁 (Property)",
	Unit = "   (Unit)",
	Value = "   (Value)",
	Enum = " 練 (Enum)",
	Keyword = "   (Keyword)",
	Snippet = "   (Snippet)",
	Color = "   (Color)",
	File = "   (File)",
	Reference = "   (Reference)",
	Folder = "   (Folder)",
	EnumMember = "   (EnumMember)",
	Constant = " ﲀ  (Constant)",
	Struct = " ﳤ  (Struct)",
	Event = "   (Event)",
	Operator = "   (Operator)",
	TypeParameter = "   (TypeParameter)",
}

-- luasnip setup
local luasnip = require("luasnip")

-- nvim cmp

local cmp = require("cmp")
cmp.setup({
	-- completion = {
	--completeopt = 'menu,menuone,noinsert'
	--},
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	mapping = {
		["<C-k>"] = function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end,

		["<C-j>"] = function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end,

		["<c-x>"] = cmp.mapping.close(),
		["<CR>"] = cmp.mapping.confirm({
			behavior = cmp.ConfirmBehavior.Replace,
			select = true,
		}),
		--["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s" }),
		--["<S-Tab>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "s" }),
	},
	formatting = {
		format = function(entry, item)
			item.kind = lsp_symbols[item.kind]
			item.menu = ({
				buffer = "[Buffer]",
				nvim_lsp = "[LSP]",
				luasnip = "[Snippet]",
				path = "[Path]",
			})[entry.source.name]

			return item
		end,
	},
	sources = { { name = "nvim_lsp" }, { name = "buffer" }, { name = "luasnip" }, { name = "path" } },
})

-- Set up telescope
local actions = require("telescope.actions")
require("telescope").setup({
	-- defaults = {
	--     mappings = {
	--         i = {
	--             ["<esc>"] = actions.close,
	--         },
	--     },
	-- },
	-- pickers = {
	-- 	current_buffer_fuzzy_find = {
	-- 		sorting_strategy = "ascending",
	-- 		layout_config = {
	-- 			prompt_position = "top",
	-- 		},
	-- 	},
	-- },
})
require("telescope").load_extension("fzf")
require("telescope").load_extension("file_browser")

-- Define local variables
local keymap = vim.api.nvim_set_keymap
local default_opts = { noremap = true, silent = true }

-- Create functions
function lf_select_current_file()
	local filename = vim.api.nvim_buf_get_name(0)
	if vim.loop.fs_stat(filename) ~= nil then
		nospacefilename = string.gsub(filename, " ", "\\ ")
		vim.cmd(':silent !lf -remote "send select ' .. nospacefilename .. '"')
		vim.cmd(':silent !~/.config/sway/scripts/togglefiles.sh')
	end
end

-- Remap leader
vim.g.mapleader = " "

-- Colorscheme
vim.o.termguicolors = true -- enable 24-bit RGB colors
vim.cmd([[colorscheme nightfly]]) -- set colorscheme

-- Global options (vim.o)
vim.o.mouse = "a"
vim.o.clipboard = "unnamedplus" -- copy/paste to system clipboard
vim.o.syntax = "enable" -- enable syntax highlighting
vim.o.number = true -- show line number
vim.o.showmatch = true -- highlight matching parenthesis
vim.o.splitright = true -- vertical split to the right
vim.o.splitbelow = true -- horizontal split to the bottom
vim.o.ignorecase = true -- ignore case letters when search
vim.o.smartcase = true -- ignore lowercase for the whole pattern

-- Local to window (vim.wo)

-- Local to buffer (vim.bo)
vim.bo.expandtab = true
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.softtabstop = 4
vim.bo.smartindent = true

-- Keybindings - saving
keymap("n", "<leader>q", ":q<CR>", default_opts)
keymap("n", "<leader>e", ":wq<CR>", default_opts)
keymap("n", "<leader>w", ":w<CR>", default_opts)

-- Keybindings - toggles
keymap("n", "<a-m>", "<c-w>w", default_opts)
-- keymap("n", "<c-h>", "<c-w>h", default_opts)
-- keymap("n", "<c-l>", "<c-w>l", default_opts)
--keymap("n", "<c-j>", "<c-w>j", default_opts)
--keymap("n", "<c-k>", "<c-w>k", default_opts)

-- Keybindings - splits
--keymap("n", "<leader>sh", ":split<CR>", default_opts)
keymap("n", "<a-v>", ":vsplit<CR>", default_opts)
keymap("n", "<a-h>", ":split<CR>", default_opts)

-- Keybindings - toggles
keymap("n", "<leader>nh", ":nohl<CR>", default_opts)
keymap("n", "<leader>nn", ":set invnumber<CR>", default_opts)

-- Keybindings - movements
keymap("n", "<A-k>", ":move -2<CR>", default_opts)
keymap("n", "<A-j>", ":move +1<CR>", default_opts)
keymap("", "<C-k>", "{zz", default_opts)
keymap("", "<C-j>", "}zz", default_opts)
keymap("", "<C-u>", "<C-u>zz", default_opts)
keymap("", "<C-d>", "<C-d>zz", default_opts)
keymap("", "<C-h>", "^", default_opts)
keymap("", "<C-l>", "$", default_opts)
keymap("", "<C-b>", "o<esc>", default_opts)
keymap("i", "<C-k>", "<up>", default_opts)
keymap("i", "<C-j>", "<down>", default_opts)
keymap("i", "<C-h>", "<left>", default_opts)
keymap("i", "<C-l>", "<Right>", default_opts)
keymap("v", "<A-k>", ":move '<-2<CR>gv=gv", default_opts)
keymap("v", "<A-j>", ":move '>+1<CR>gv=gv", default_opts)
keymap("n", "n", "nzz", default_opts)
keymap("n", "N", "Nzz", default_opts)
keymap("n", "J", "mzJ`z", default_opts)

keymap("", "<A-p>", ":cprev<CR>", default_opts)
keymap("", "<A-n>", ":cnext<CR>", default_opts)

-- Keybindings - formatting
keymap("n", "<leader>id", ":-r !dateheader.sh<CR>", default_opts)
keymap("n", "<leader>ih", "<cmd>s/^/==\\ /<CR><cmd>s/$/\\ ==/<CR>:nohl<CR>", default_opts)
keymap("n", "<leader>il", "<cmd>s/^/[[/<CR><cmd>s/$/]]/<CR>:nohl<CR>", default_opts)
keymap("n", "<leader>ib", "i#!/usr/bin/", default_opts)

-- Keybindings - misc
keymap("n", "<leader>lf", "<cmd>lua lf_select_current_file()<CR>", default_opts)
keymap("i", "<c-b>", "<cmd>normal o<CR>", default_opts)

-- Keybindings - telescope
keymap(
	"n",
	"<leader>of",
	'<cmd>lua require("telescope.builtin").find_files({find_command={"find", "-L", HOME, "(", "-path", HOME .. "/.cache", "-o", "-path", HOME .. "/.mozilla", "-o", "-path", HOME .. "/.thunderbird", "-o", "-path", HOME .. "/.Mail", "-o", "-path", HOME .. "/media", "-o", "-path", HOME .. "/downloads", "-o", "-path", HOME .. "/my_docs", "-o", "-path", HOME .. "/work_docs", "-o", "-path", HOME .. "/.archivedmail", "-o", "-path", HOME .. "/.local", "-o", "-path", HOME .. "/.cargo", "-o", "-path", HOME .. "/.thumbnails", "-o", "-path", HOME .. "/.npm", "-o", "-path", HOME .. "/.test/envs", "-o", "-path", HOME .. "/.Private", "-o", "-path", HOME .. "/.zoom", ")", "-prune", "-o", "-type", "f", "-print"}})<CR>',
	default_opts
)
keymap(
	"n",
	"<leader>on",
	'<cmd>lua require("telescope.builtin").find_files({cwd=HOME .. "/notes/"})<CR>',
	default_opts
)

keymap("n", "<leader>fb", '<cmd>Telescope file_browser<CR>', default_opts)
keymap("n", "<leader>ft", '<cmd>lua require("telescope.builtin").filetypes()<CR>', default_opts)
keymap("n", "<leader>h", '<cmd>lua require("telescope.builtin").help_tags()<CR>', default_opts)
keymap("n", "<leader>dd", '<cmd>lua require("telescope.builtin").lsp_document_diagnostics()<CR>', default_opts)
keymap("n", "<leader>ds", '<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>', default_opts)

keymap("n", "sb", '<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>', default_opts)
keymap("n", "<leader>lg", '<cmd>lua require("telescope.builtin").live_grep()<CR>', default_opts)
keymap(
	"n",
	"<leader>sn",
	'<cmd>lua require("telescope.builtin").live_grep({cwd=HOME .. "/notes/"})<CR>',
	default_opts
)

Changebuf = function()
	vim.cmd("w")
	require("telescope.builtin").buffers()
end

-- Keybindings - buffers
keymap("n", "<leader>ls", "<cmd>lua Changebuf()<CR>", default_opts)
keymap("n", "<c-n>", "<cmd>bn<CR>", default_opts)
keymap("n", "<c-p>", "<cmd>bp<CR>", default_opts)
keymap("n", "<leader>bd", "<cmd>bd<CR>", default_opts)

-- functions for terminal compile / run

Mycr = vim.api.nvim_replace_termcodes("<CR>", true, false, true)
--mysend = function()
--    vim.cmd('vsp | terminal')
--    vim.fn.chansend(vim.b.terminal_job_id, "echo \"hi\"")
--    vim.fn.chansend(vim.b.terminal_job_id, mycr)
--end

function Split(inputstr, sep)
	if sep == nil then
		sep = "%s"
	end
	local t = {}
	i = 1
	for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
		t[i] = str
		i = i + 1
	end
	return t
end

getfilenames = function()
	if vim.bo.buftype == "" then
		mybufnr = vim.fn.bufnr("%")
		mybufwinnr = vim.fn.bufwinnr(mybufnr)

		fullpathname = vim.api.nvim_buf_get_name(0)
		vim.cmd("w")

		if vim.loop.fs_stat(fullpathname) ~= nil then
			local fileparts = { string.match(fullpathname, "(.-)([^\\/]-%.?([^%.\\/]*))$") }
			local filepath = fileparts[1]
			local filename = fileparts[2]
			local extension = fileparts[3]
			function runscript(job)
				getaterm()
				vim.fn.chansend(vim.b.terminal_job_id, "cd " .. filepath .. Mycr)
				vim.fn.chansend(
					vim.b.terminal_job_id,
					"[ ! -x " .. fullpathname .. " ] && chmod u+x " .. fullpathname .. Mycr
				)
				vim.fn.chansend(vim.b.terminal_job_id, job .. filename .. Mycr)
			end
			if extension == "lua" then
				runscript("lua ./")
			elseif extension == "py" then
				runscript("python ./")
			elseif extension == "sh" then
				runscript("./")
			elseif extension == "c" then
				getaterm()
				local splitname = Split(filename, ".")
				local noext = splitname[1]
				vim.fn.chansend(vim.b.terminal_job_id, "cd " .. filepath .. Mycr)
				vim.fn.chansend(vim.b.terminal_job_id, "gcc -Wall " .. fullpathname .. " -o " .. noext .. Mycr)
				if vim.loop.fs_stat(noext) ~= nil then
					vim.fn.chansend(vim.b.terminal_job_id, "[ $? -eq 0 ] && ./" .. noext .. Mycr)
				end
			elseif extension == "md" then
				newfilename = string.gsub(fullpathname, " ", "\\ ")
				vim.cmd("silent !topdf.sh " .. newfilename)
			end
		end
	end
	vim.cmd("startinsert")
	vim.cmd("stopinsert")
	vim.cmd("exe " .. mybufwinnr .. ' . "wincmd w"')
	--     vim.cmd('stopinsert')
end

getaterm = function()
	local winvalue = vim.fn.bufwinnr("term://")
	if winvalue > 0 then
		vim.cmd("exe " .. winvalue .. ' . "wincmd w"')
	else
		vim.cmd("sp | terminal")
	end
end

myopenterm = function()
	getaterm()
	vim.cmd("startinsert")
end

mycloseterm = function()
	local termlist = vim.api.nvim_eval(
		"filter(map(getbufinfo(), 'v:val.bufnr'), 'getbufvar(v:val, \"&buftype\") is# \"terminal\"')"
	)
	if #termlist > 0 then
		termbufno = termlist[1]
		vim.cmd("bd! " .. termbufno)
	end
end

-- Keybindings - terminal
keymap("t", "<Esc>", "<C-\\><C-N>", default_opts)
keymap("n", "<F10>", "<cmd>lua getfilenames()<CR>", default_opts)
keymap("n", "<F6>", "<cmd>lua myopenterm()<CR>", default_opts)
keymap("t", "<F7>", "<cmd>lua mycloseterm()<CR>", default_opts)
keymap("n", "<F7>", "<cmd>lua mycloseterm()<CR>", default_opts)

-- formatter
keymap("n", "<F5>", "<Cmd>lua vim.lsp.buf.formatting()<CR>", default_opts)

-- autocommands
--vim.api.nvim_create_autocmd("BufEnter", { command = "echo 'Hello'", })
--https://www.youtube.com/watch?v=ekMIIAqTZ34

