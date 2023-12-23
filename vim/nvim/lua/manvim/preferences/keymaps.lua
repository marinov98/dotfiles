vim.g.mapleader = " "

vim.keymap.set('n', '<leader>s',":w <CR>", { desc = "Save File"})
vim.keymap.set('n', '<leader>q',":q <CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q',":q! <CR>", { desc = "Force Quit" })
