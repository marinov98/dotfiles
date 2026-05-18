vim.g.mapleader = " "
vim.g.maplocalleader = ","

local map = vim.keymap.set
-- Saving, Quitting, Navigating
map('n', '<leader>a', "<C-^>", { desc = "Alternate buffers" })
map('n', '<leader>s', ":w<CR>", { desc = "Save File" })
map('n', '<leader>k', ":bd<CR>", { desc = "Close buffer" })
map('n', '<leader>K', ":bd!<CR>", { desc = "(Force) Close buffer" })
map('n', '<leader>x', ":tabclose<CR>", { desc = "Close tab" })
map('n', '<leader>q', ":q<CR>", { desc = "Quit" })
map('n', '<leader>Q', ":q!<CR>", { desc = "Force Quit" })

-- Coding utility
map('n', '<leader>cs', function()
  vim.cmd.vnew()
  vim.cmd.term()
  vim.cmd.wincmd("J")
  vim.api.nvim_win_set_height(0, 15)
end, { desc = "Open terminal at bottom" })
map('n', '<leader>cn', ":noh<CR>", { desc = "Remove highlight" })
map("n", "<leader>ca", ":%s/<C-r><C-w>/<C-r><C-w>/g<Left><Left>",
  { desc = "Change all word under cursor with confirmation" })
map("n", "<leader>cA", ":%s/<C-r><C-a>/<C-r><C-a>/g<Left><Left>",
  { desc = "Change all WORD under cursor with confirmation" })
map("x", "<leader>cA", ":s/$//<Left>", { desc = "Edit end of every line in visual selection" })
map("n", "<leader>ci", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  { desc = "Case insensitive search/replace under cursor for current file" })

-- General Utility
map("n", "<leader>ux", "<cmd>!chmod +x %<CR>", { silent = true, desc = "Chmod +x current file" })
map("n", "<leader>uc", ":! ", { silent = true, desc = "Execute external command" })
-- map('n', '-', vim.cmd.Explore, { desc = "Open file browser" })
-- map('n', '<leader>ut', ":Vexplore!<CR>", { desc = "Open netrw side bar" })
