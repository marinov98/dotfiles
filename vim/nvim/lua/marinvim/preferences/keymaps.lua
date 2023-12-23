vim.g.mapleader = " "

vim.keymap.set('n', '<leader>s',":w <CR>", { desc = "Save File"})
vim.keymap.set('n', '<leader>q',":q <CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q',":q! <CR>", { desc = "Force Quit" })
vim.keymap.set('n', '<leader>n',":noh <CR>", { desc = "Remove highlight" })

-- ONLY if hop plugin is used! (These cause error if put inside the plugin itself)
vim.keymap.set('n', '<leader>ag',":HopChar1<CR>", { desc = "Goto Char 1"})
vim.keymap.set('n', '<leader>as',":HopChar2<CR>", { desc = "Goto Char 2"})
vim.keymap.set('n', '<leader>at',":HopPattern<CR>", { desc = "Goto pattern"})
vim.keymap.set('n', '<leader>aw',":HopWord<CR>", { desc = "Goto Word"})
