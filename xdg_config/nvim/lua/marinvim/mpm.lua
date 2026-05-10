local M = {}
local map = vim.keymap.set

--- Closes all loaded buffers except for the current one.
--- Iterates through all buffers, checks if they are loaded and not the current
--- buffer, and then deletes them. It prints a message indicating how many
--- buffers were closed.
--- This function will not close buffers that are not loaded.
--- @param opts table
--- @return nil
function M.close_other_buffers(opts)
  opts = opts or {}
  local curr_buf = vim.api.nvim_get_current_buf()
  local bufs = vim.api.nvim_list_bufs()
  for _, buf in ipairs(bufs) do
    if vim.api.nvim_buf_is_loaded(buf) and buf ~= curr_buf then
      vim.api.nvim_buf_delete(buf, opts)
    end
  end
  print(#bufs - #vim.api.nvim_list_bufs(), "buffer(s)", opts.force and "(force) deleted" or "deleted")
end

function M.toggle_signcolumns()
  local curr = vim.opt.signcolumn:get()
  if curr == 'no' then
    vim.cmd("set signcolumn=yes")
  else
    vim.cmd("set signcolumn=no")
  end
end

function M.get_visual_selection()
  local old_text = vim.fn.getreg('c')
  vim.cmd('normal! "cy')

  local yanked_text = vim.fn.getreg('c')

  vim.fn.setreg('c', old_text)
  return yanked_text
end

function M.mpm_Vedit(opts)
  opts = opts or {}
  local escaped = vim.fn.escape(M.get_visual_selection(), "/\\")
  vim.fn.setreg("/", "\\V" .. escaped)
  vim.api.nvim_feedkeys(opts.reverse and "cgN" or "cgn", "n", false)
end

function M.mpm_Medit()
  map("n", "gl", "*Ncgn", { desc = "(Multi Edit) Change occurence under cursor" })
  map("n", "gL", "*NcgN", { desc = "(Multi Edit) Change occurence under cursor" })

  map("x", "gl", M.mpm_Vedit, { desc = "Multi edit visually selected occurence forward" })
  map("x", "gL", function() M.mpm_Vedit({ reverse = true }) end,
    { desc = "Multi edit visually selected occurence reverse" })

  map("x", "<leader>ca", function()
    local target = M.get_visual_selection()
    vim.fn.feedkeys(":%s/" .. target .. "/" .. target .. "/g\x80kl\x80kl", "n")
  end, { desc = "Edit all patterns in visual selection" })
end

function M.setup(opts)
  opts = opts or {}

  -- Buffers
  map("n", "<leader><leader>k", M.close_other_buffers, { desc = "Close all buffers but the current one" })
  map("n", "<leader><leader>K", function() M.close_other_buffers({ force = true }) end,
    { desc = "Force close all buffers but the current one" })

  -- SignColumns
  map("n", "<leader>uS", M.toggle_signcolumns, { desc = "Toggle sign columns" })

  M.mpm_Medit()
end

return M
