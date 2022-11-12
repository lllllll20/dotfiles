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
  buf_set_keymap("n", "<leader>ds", '<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>', opts)
  -- buf_set_keymap("n", "<leader>dd", '<cmd>lua require("telescope.builtin").lsp_document_diagnostics()<CR>', opts)
  buf_set_keymap("n", "<leader>dd", '<cmd>lua require("telescope.builtin").diagnostics()<CR>', opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

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


