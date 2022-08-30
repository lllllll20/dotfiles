closenotes = function()
  local buffers = vim.tbl_filter(
      function(buf) return
          vim.api.nvim_buf_is_valid(buf) and vim.bo[buf].buflisted
      end,
      vim.api.nvim_list_bufs()
  )
  local handle = io.popen("find \"${HOME}/notes/Radiology notes\" \\( -path \"${HOME}/notes/Radiology notes/Second Year\" -o -path \"${HOME}/notes/Radiology notes/Third Year\" -o -path \"${HOME}/notes/Radiology notes/Exams/2A Lists.md\" \\) -prune -o -type f -print")
  if handle then for line in handle:lines() do
    for _,j in ipairs(buffers) do
      if vim.api.nvim_buf_is_valid(j) and vim.bo[j].buflisted then
        if (line == vim.api.nvim_buf_get_name(j)) then
          vim.api.nvim_buf_delete(j, {})
        end
      end
    end
  end
  handle:close()
  end
end



local HOME = os.getenv("HOME")

-- function getdirs()
--   local data = require("plenary.scandir").scan_dir(HOME .. "/notes/Radiology notes/Exams", {
--     hidden = true,
--     search_pattern = "2B"
--   })
--   for _, entry in ipairs(data) do
--   print(entry)
--   end
-- end
--
--
--
--
-- vim.keymap.set("n", "<leader>pa", "<cmd>lua getdirs()<cr>")
