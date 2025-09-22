local M = {}

-- The function to get the visual selection and return the search string
function M.get_visual_selection()
  vim.api.nvim_feedkeys('\027', 'xt', false)
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")

  local lines = vim.api.nvim_buf_get_lines(
    0,
    start_pos[2] - 1,
    end_pos[2],
    false
  )

  if not lines or #lines == 0 then
    return ""
  end

  -- The multi-line check to ensure it's a single-line selection
  if start_pos[2] ~= end_pos[2] then
    vim.notify("Multi-line selection not supported", vim.log.levels.INFO)
    return ""
  end

  local search_string = string.sub(lines[1], start_pos[3], end_pos[3]) or ""
  if search_string == nil or search_string == "" then
    print("Empty/nil string from visual selection!")
  end

  return search_string
end

return M
