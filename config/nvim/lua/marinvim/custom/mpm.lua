local M = {}

--- Gets the currently selected text from a single-line visual selection.
-- This function first escapes from any active selection, then retrieves the start
-- and end positions of the last visual selection. It checks to ensure the
-- selection is on a single line and returns an empty string if not.
-- The function returns the selected text as a string.
--
-- @returns {string} The text from the visual selection, or an empty string if
--                   the selection is multi-line or no text is selected.
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

  return string.sub(lines[1], start_pos[3], end_pos[3])
end

return M
