vim.g.autopair=true

local keymap = vim.api.nvim_set_keymap
local default_opts = { noremap = true, silent = true }
keymap("n", "<leader>ib", "i#!/usr/bin/env python", default_opts)
