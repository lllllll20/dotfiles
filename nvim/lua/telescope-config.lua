local actions = require("telescope.actions")
require("telescope").setup({
  defaults = {
    -- Default configuration for telescope goes here:
    -- config_key = value,
    mappings = {
      n = {
          ['<c-d>'] = require('telescope.actions').delete_buffer
      }, -- n
      i = {
        ['<c-d>'] = require('telescope.actions').delete_buffer
      } -- i
    } -- mappings
  }, -- defaults
} -- telescope setup
)
require("telescope").load_extension("fzf")
