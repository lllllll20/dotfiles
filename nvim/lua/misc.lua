-- vim.api.nvim_create_autocmd("BufEnter",
--   { callback = function() if vim.bo.buftype == "terminal" then
--     vim.fn.feedkeys('G', 't')
--     vim.cmd("startinsert")
--   end
-- end })

-- Create functions
function lf_select_current_file()
  local filename = vim.api.nvim_buf_get_name(0)
  if vim.loop.fs_stat(filename) ~= nil then
    nospacefilename = string.gsub(filename, " ", "\\ ")
    vim.cmd(':silent !lf -remote "send select ' .. nospacefilename .. '"')
    vim.cmd(":silent !~/.config/sway/scripts/togglefiles.sh")
  end
end

Changebuf = function()
  vim.cmd("w")
  require("telescope.builtin").buffers()
end

-- functions for terminal compile / run

local Mycr = vim.api.nvim_replace_termcodes("<CR>", true, false, true)
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
  local startpos = vim.api.nvim_get_current_win()
  if vim.bo.buftype == "" then
    mybufnr = vim.fn.bufnr("%")
    mybufwinnr = vim.fn.bufwinnr(mybufnr)
    -- vim.api.nvim_get_current_win()
    -- vim.api.nvim_get_current_buf()
    -- vim.api.nvim_set_current_win()
    -- vim.api.nvim_set_current_buf()
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
      elseif extension == "rs" then
        getaterm()
        vim.fn.chansend(vim.b.terminal_job_id, "cd " .. filepath .. Mycr)
        vim.fn.chansend(vim.b.terminal_job_id, "cargo run" .. Mycr)
      elseif extension == "py" then
        getaterm()
        vim.fn.chansend(vim.b.terminal_job_id, "source " .. HOME .. "/.test/envs/plain/bin/activate" .. Mycr)
        vim.fn.chansend(vim.b.terminal_job_id, "cd " .. filepath .. Mycr)
        vim.fn.chansend(
          vim.b.terminal_job_id,
          "[ ! -x " .. fullpathname .. " ] && chmod u+x " .. fullpathname .. Mycr
        )
        vim.fn.chansend(vim.b.terminal_job_id, "python ./" .. filename .. Mycr)
      elseif extension == "sh" then
        runscript("./")
      elseif extension == "c" then
        getaterm()
        local splitname = Split(filename, ".")
        local noext = splitname[1]
        vim.fn.chansend(vim.b.terminal_job_id, "cd " .. filepath .. Mycr)
        vim.fn.chansend(vim.b.terminal_job_id, "gcc -Wall " .. fullpathname .. " -o " .. noext .. Mycr)
        if vim.loop.fs_stat(filepath .. noext) ~= nil then
          vim.fn.chansend(vim.b.terminal_job_id, "[ $? -eq 0 ] && ./" .. noext .. Mycr)
        end
      elseif extension == "md" then
        if string.match(fullpathname, "%/%w+%/%w+%/%w+") == HOME .. "/notes" then
          newfilename = string.gsub(fullpathname, " ", "\\ ")
          vim.cmd("silent !topdf.sh " .. newfilename)
        else
          newfilename = string.gsub(fullpathname, " ", "\\ ")
          vim.cmd("silent !topdf2.sh " .. newfilename)
        end
      end
    end
  end
  local termlist =
  vim.api.nvim_eval("filter(map(getbufinfo(), 'v:val.bufnr'), 'getbufvar(v:val, \"&buftype\") is# \"terminal\"')")
  if #termlist > 0 then
    local termbufno = termlist[1]
    vim.api.nvim_buf_call(termbufno, function() vim.cmd("norm G") end)
  end
  vim.api.nvim_set_current_win(startpos)
  --vim.cmd("startinsert")
  --vim.cmd("stopinsert")
  -- jumps back to the original buffer window
  --vim.cmd("exe " .. mybufwinnr .. ' . "wincmd w"')
  --     vim.cmd('stopinsert')
end

getaterm = function()
  local winvalue = vim.fn.bufwinnr("term://")
  if winvalue > 0 then
    local winid = vim.fn.win_getid(winvalue)
    vim.api.nvim_set_current_win(winid)
  else
    local win_height = vim.api.nvim_win_get_height(0)
    local win_width = vim.api.nvim_win_get_width(0)
    if (5 * win_height) > win_width then
      vim.cmd("sp | terminal")
    else
      vim.cmd("vsp | terminal")
    end
  end
end

myopenterm = function()
  getaterm()
  vim.cmd("startinsert")
end

mycloseterm = function()
  local termlist =
  vim.api.nvim_eval("filter(map(getbufinfo(), 'v:val.bufnr'), 'getbufvar(v:val, \"&buftype\") is# \"terminal\"')")
  if #termlist > 0 then
    termbufno = termlist[1]
    vim.cmd("bd! " .. termbufno)
  end
end
--vim.fn.feedkeys('G', 't')
--vim.cmd("normal! G")
--vim.fn.win_execute(vim.api.nvim_get_current_win(), "normal! G")
--vim.api.nvim_buf_call(bufnr, function() vim.cmd("norm G") end)
