local M = {}

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

-- To be used if no other file tree plugins exist
function M.enable_netrw_keymaps()
  vim.keymap.set('n', '-', vim.cmd.Explore, { desc = "Open file browser" })
  vim.keymap.set('n', '<leader>ut', ":Vexplore!<CR>", { desc = "Open netrw side bar" })
end

function M.set_hl()
  vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
end

function M.get_visual_selection()
  local old_text = vim.fn.getreg('c')
  vim.cmd('normal! "cy')

  local yanked_text = vim.fn.getreg('c')

  vim.fn.setreg('c', old_text)
  return yanked_text
end

function M.setup(opts)
  opts = opts or {}

  -- Buffers
  vim.keymap.set("n", "<leader><leader>k", M.close_other_buffers,
    { desc = "Close all buffers but the current one" })
  vim.keymap.set("n", "<leader><leader>K", function()
    M.close_other_buffers({ force = true })
  end, { desc = "Force close all buffers but the current one" })

  -- SignColumns
  vim.keymap.set("n", "<leader>uS", M.toggle_signcolumns, { desc = "Toggle sign columns" })

  -- Copying, Pasting
  if vim.opt.clipboard._value ~= 'unnamedplus' then
    vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy to system clipboard" })
    vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line to system clipboard" })
    vim.keymap.set({ "n", "v" }, "<leader>p", [["+p]], { desc = "Paste from system clipboard" })
  end

  -- Search/Replace
  vim.keymap.set("x", "<leader>ca", function()
    local target = M.get_visual_selection()
    vim.fn.feedkeys(":%s/" .. target .. "/" .. target .. "/gc\x80kl\x80kl\x80kl", "n")
  end, { desc = "Change all selections in file with confirmation" })

  -- File Tree
  -- M.enable_netrw_keymaps()

  -- Highlight
  -- M.set_hl()
end

return M
