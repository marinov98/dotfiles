vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Saving, Quitting, Navigating
vim.keymap.set('n', '<leader>a', "<C-^>", { desc = "Alternate buffers" })
vim.keymap.set('n', '<leader>s', ":w<CR>", { desc = "Save File" })
vim.keymap.set('n', '<leader>k', ":bd<CR>", { desc = "Close buffer" })
vim.keymap.set('n', '<leader>x', ":tabclose<CR>", { desc = "Close tab" })
vim.keymap.set('n', '<leader>q', ":q<CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q', ":q!<CR>", { desc = "Force Quit" })

-- Coding utility
vim.keymap.set('n', '<leader>cs', ":terminal<CR>", { desc = "Open terminal" })
vim.keymap.set('n', '<leader>cn', ":noh<CR>", { desc = "Remove highlight" })
vim.keymap.set("n", "<leader>ca", ":%s/<C-r><C-w>/<C-r><C-w>/g<Left><Left>",
  { desc = "Change all word under cursor with confirmation" })
vim.keymap.set("n", "<leader>cA", ":%s/<C-r><C-a>/<C-r><C-a>/g<Left><Left>",
  { desc = "Change all WORD under cursor with confirmation" })
vim.keymap.set("n", "<leader>ci", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]],
  { desc = "Case insensitive search/replace under cursor for current file" })

-- General Utility
vim.keymap.set("n", "<leader>ux", "<cmd>!chmod +x %<CR>", { silent = true, desc = "Chmod +x current file" })
vim.keymap.set("n", "<leader>uc", ":! ", { silent = true, desc = "Execute external command" })
