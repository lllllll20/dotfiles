-- ENVS
HOME = os.getenv("HOME")

-- Global options (vim.o)
vim.o.mouse = "a"
--vim.o.clipboard = "unnamedplus" -- copy/paste to system clipboard
vim.o.clipboard = "" -- copy/paste to system clipboard
vim.o.syntax = "enable" -- enable syntax highlighting
vim.o.rnu = true
vim.o.splitbelow = true -- horizontal split to the bottom
vim.o.ignorecase = true -- ignore case letters when search
vim.o.smartcase = true -- ignore lowercase for the whole pattern
vim.o.laststatus = 3 -- ignore lowercase for the whole pattern
vim.cmd([[highlight WinSeparator guibg=None]]) -- set colorscheme

-- Local to window (vim.wo)

-- Local to buffer (vim.bo)
vim.bo.expandtab = true
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.softtabstop = 4
vim.bo.smartindent = true
vim.bo.textwidth = 80

-- Colorscheme
vim.o.termguicolors = true -- enable 24-bit RGB colors
vim.cmd([[colorscheme nightfly]]) -- set colorscheme

-- Remap leader
vim.g.mapleader = " "
