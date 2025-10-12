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

function M.setup(opts)
  opts = opts or {}

  vim.keymap.set("n", "<leader><leader>k", M.close_other_buffers,
    { desc = "Close all buffers but the current one" })
  vim.keymap.set("n", "<leader><leader>K", function()
    M.close_other_buffers({ force = true })
  end, { desc = "Force close all buffers but the current one" })
end

return M
