local mpm = require("marinvim.custom.mpm")
local map = vim.keymap.set

map("n", "gl", "*Ncgn", { desc = "(Multi Edit) Change occurence under cursor" })
map("n", "gL", "*NcgN", { desc = "(Multi Edit) Change occurence under cursor" })

map("x", "gl", mpm.visual_multiedit, { desc = "Multi edit visually selected occurence forward" })
map("x", "gL", function() mpm.visual_multiedit({ reverse = true }) end,
  { desc = "Multi edit visually selected occurence reverse" })

map("x", "<leader>ca", function()
  local target = mpm.get_visual_selection()
  vim.fn.feedkeys(":%s/" .. target .. "/" .. target .. "/g\x80kl\x80kl", "n")
end, { desc = "Edit all patterns in visual selection" })


map("n", "<leader><leader>k", mpm.close_other_buffers, { desc = "Close all buffers but the current one" })
map("n", "<leader><leader>K", function() mpm.close_other_buffers({ force = true }) end,
  { desc = "Force close all buffers but the current one" })

local picker = require("snacks").picker
map("n", "gf", function()
  picker.files({ search = vim.fn.expand("<cWORD>"), args = { "--full-path" } })
end, { desc = "Find file under cursor" })
map("x", "gf", function()
  picker.files({ search = mpm.get_visual_selection(), args = { "--full-path" } })
end, { desc = "Find file from visual selection" })
