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
  print(#bufs - #vim.api.nvim_list_bufs(), "buffer(s) deleted")
end

--- A Lua function to yank the current visual selection, store it in register 'c',
--- and then return the yanked text. The 'c' register is cleared after the
--- text is retrieved.
--- @return string
function M.get_visual_selection()
    -- Execute a normal mode command to yank the selection into register 'c'.
    -- We can assume the function is always called from visual mode.
    local old_text = vim.fn.getreg('c')
    vim.cmd('normal! "cy')

    local yanked_text = vim.fn.getreg('c')

    vim.fn.setreg('c', old_text)

    return yanked_text
end

return M
