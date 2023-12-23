vim.g.mapleader = " "

-- saving, quitting
vim.keymap.set('n', '<leader>s',":w<CR>", { desc = "Save File"})
vim.keymap.set('n', '<leader>k',":tabclose<CR>", { desc = "Close tab" })
vim.keymap.set('n', '<leader>q',":q<CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q',":q!<CR>", { desc = "Force Quit" })
vim.keymap.set('n', '<leader>n',":noh<CR>", { desc = "Remove highlight" })

-- Buffers and tabs
vim.keymap.set('n', '<leader>bb',"<C-^>", { desc = "Alternate buffers" })
vim.keymap.set('n', '<leader>bt',":tabnew<CR>", { desc = "New Tab" })
vim.keymap.set('n', '<leader>bn',":tabnext<CR>", { desc = "Next Tab" })
vim.keymap.set('n', '<leader>bp',":tabprevious<CR>", { desc = "Next Tab" })

-- ONLY if hop plugin is used! (These cause error if put inside the plugin itself)
vim.keymap.set('n', '<leader>ag',":HopChar1<CR>", { desc = "Goto Char 1"})
vim.keymap.set('n', '<leader>as',":HopChar2<CR>", { desc = "Goto Char 2"})
vim.keymap.set('n', '<leader>at',":HopPattern<CR>", { desc = "Goto pattern"})
vim.keymap.set('n', '<leader>aw',":HopWord<CR>", { desc = "Goto Word"})
