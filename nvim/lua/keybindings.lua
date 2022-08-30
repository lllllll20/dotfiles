-- Define local variables
local keymap = vim.api.nvim_set_keymap
local default_opts = { noremap = true, silent = true }

-- Keybindings - saving
keymap("n", "<leader>q", ":q<CR>", default_opts)
keymap("n", "<leader>e", ":wq<CR>", default_opts)
keymap("n", "<leader>w", ":w<CR>", default_opts)

-- Keybindings - window navigation
keymap("n", "<a-c>", "<c-w>w", default_opts)

-- Keybindings - splits
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
keymap("", "<C-;>", "o<esc>", default_opts)
keymap("v", "<A-k>", ":move '<-2<CR>gv=gv", default_opts)
keymap("v", "<A-j>", ":move '>+1<CR>gv=gv", default_opts)
keymap("n", "n", "nzz", default_opts)
keymap("n", "N", "Nzz", default_opts)
keymap("n", "J", "mzJ`z", default_opts)
keymap("i", "<C-l>", "<Right>", default_opts)
keymap("i", "<C-h>", "<Left>", default_opts)

-- Keybindings - quickfix
keymap("", "<c-p>", ":cprev<CR>", default_opts)
keymap("", "<c-n>", ":cnext<CR>", default_opts)

-- Keybindings - formatting
keymap("n", "<leader>id", ":-r !dateheader.sh<CR>", default_opts)
keymap("n", "<leader>ih", "<cmd>s/^/==\\ /<CR><cmd>s/$/\\ ==/<CR>:nohl<CR>", default_opts)
keymap("n", "<leader>il", "<cmd>s/^/[[/<CR><cmd>s/$/]]/<CR>:nohl<CR>", default_opts)
keymap("n", "<leader>ib", "i#!/usr/bin/", default_opts)

-- Keybindings - misc
keymap("n", "<leader>f", "<cmd>lua lf_select_current_file()<CR>", default_opts)
keymap("n", "<c-f>", "<cmd>lua lf_select_current_file()<CR>", default_opts)
keymap("n", "<f2>", "<cmd>lua lf_select_current_file()<CR>", default_opts)
keymap("i", "<c-b>", "<cmd>normal o<CR>", default_opts)
keymap("i", "<c-;>", "<cmd>normal o<CR>", default_opts)

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

keymap("n", "<leader>h", '<cmd>lua require("telescope.builtin").help_tags()<CR>', default_opts)

keymap("n", "sb", '<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>', default_opts)
keymap("n", "<leader>lg", '<cmd>lua require("telescope.builtin").live_grep()<CR>', default_opts)
keymap(
	"n",
	"<leader>sn",
	'<cmd>lua require("telescope.builtin").live_grep({cwd=HOME .. "/notes/"})<CR>',
	default_opts
)


-- Keybindings - terminal
keymap("t", "<Esc>", "<C-\\><C-N>", default_opts)
keymap("n", "<F10>", "<cmd>lua getfilenames()<CR>", default_opts)
keymap("n", "<F6>", "<cmd>lua myopenterm()<CR>", default_opts)
keymap("t", "<F7>", "<cmd>lua mycloseterm()<CR>", default_opts)
keymap("n", "<F7>", "<cmd>lua mycloseterm()<CR>", default_opts)

-- formatter
keymap("n", "<F5>", "<Cmd>lua vim.lsp.buf.formatting()<CR>", default_opts)

-- Keybindings - buffers
keymap("n", "<leader>ls", "<cmd>lua Changebuf()<CR>", default_opts)
keymap("n", "<a-n>", "<cmd>bn<CR>", default_opts)
keymap("n", "<a-p>", "<cmd>bp<CR>", default_opts)
keymap("n", "<leader>bd", "<cmd>bd<CR>", default_opts)
keymap("n", "<leader>cn", "<cmd>lua closenotes()<CR>", default_opts)
keymap("n", "<leader>sf", ":luafile $MYVIMRC<CR>", default_opts)
