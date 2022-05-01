vim.bo.expandtab = true
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.softtabstop = 4
vim.bo.smartindent = true

require'cmp'.setup.buffer{
  completion = {
    autocomplete = false
  }
}

