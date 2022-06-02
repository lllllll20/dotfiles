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
	if vim.bo.buftype == "" then
		mybufnr = vim.fn.bufnr("%")
		mybufwinnr = vim.fn.bufwinnr(mybufnr)

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
				newfilename = string.gsub(fullpathname, " ", "\\ ")
				vim.cmd("silent !topdf.sh " .. newfilename)
			end
		end
	end
	vim.cmd("startinsert")
	vim.cmd("stopinsert")
	vim.cmd("exe " .. mybufwinnr .. ' . "wincmd w"')
	--     vim.cmd('stopinsert')
end

getaterm = function()
	local winvalue = vim.fn.bufwinnr("term://")
	if winvalue > 0 then
		vim.cmd("exe " .. winvalue .. ' . "wincmd w"')
	else
		vim.cmd("sp | terminal")
	end
end

myopenterm = function()
	getaterm()
	vim.cmd("startinsert")
end

mycloseterm = function()
	local termlist = vim.api.nvim_eval(
		"filter(map(getbufinfo(), 'v:val.bufnr'), 'getbufvar(v:val, \"&buftype\") is# \"terminal\"')"
	)
	if #termlist > 0 then
		termbufno = termlist[1]
		vim.cmd("bd! " .. termbufno)
	end
end
