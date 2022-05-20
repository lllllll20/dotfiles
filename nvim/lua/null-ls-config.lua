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
