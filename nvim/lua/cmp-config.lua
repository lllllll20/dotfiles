local status_ok, cmp = pcall(require, "cmp")
if not status_ok then return end

-- config for copilot
-- vim.g.copilot_no_tab_map = true
-- vim.g.copilot_assume_mapped = true
-- vim.g.copilot_tab_fallback = ""

local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
-- load snippets
local luasnip = require("luasnip")
--require("luasnip.loaders.from_vscode").lazy_load()
--require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets/" })

-- nvim-cmp setup
local lspkind = require('lspkind')
cmp.setup {
    snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
    formatting = {
        format = lspkind.cmp_format({
            mode = "symbol_text",
            menu = ({
                buffer = "[BUFFER]",
                nvim_lsp = "[LSP]",
                luasnip = "[LUASNIP]",
                -- cmp_tabnine = "[TN]",
                copilot = "[COPILOT]",
                path = "[PATH]"
            })
        })
    },
    mapping = cmp.mapping.preset.insert {
        ['<C-k>'] = cmp.mapping.select_prev_item(),
        ['<C-j>'] = cmp.mapping.select_next_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = false
        },
        ['<C-j>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_locally_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end, { "i", "s" }),
        ['<C-k>'] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end, { "i", "s" }),
    },
    sources = {
        { name = 'nvim_lsp' }, { name = 'nvim_lsp_signature_help' },
        { name = 'luasnip' }, { name = 'copilot' }, { name = "path" },
        { name = 'buffer' }, { name = 'nvim_lua' }
    },
    window = {
        documentation = {
            border = 'rounded',
        },

        completion = {
            border = 'rounded',
        },
    },
    experimental = { ghost_text = true }
}
