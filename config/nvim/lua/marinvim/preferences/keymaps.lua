vim.g.mapleader = " "

-- saving, quitting
vim.keymap.set('n', '<leader>s', ":w<CR>", { desc = "Save File" })
vim.keymap.set('n', '<leader>k', ":bd<CR>", { desc = "Close buffer" })
vim.keymap.set('n', '<leader>x', ":tabclose<CR>", { desc = "Close tab" })
vim.keymap.set('n', '<leader>q', ":q<CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q', ":q!<CR>", { desc = "Force Quit" })
vim.keymap.set('n', '<leader>n', ":noh<CR>", { desc = "Remove highlight" })

-- Buffers and tabs
vim.keymap.set('n', '<leader>a', "<C-^>", { desc = "Alternate buffers" })
vim.keymap.set('n', '<leader>bp', ":bprevious<CR>", { desc = "Previous Buffer" })
vim.keymap.set('n', '<leader>bn', ":bnext<CR>", { desc = "Next buffer" })

vim.keymap.set('n', '<leader>tt', ":tabnew<CR>", { desc = "New Tab" })
vim.keymap.set('n', '<leader>tj', ":tabnext<CR>", { desc = "Next Tab" })
vim.keymap.set('n', '<leader>tk', ":tabprevious<CR>", { desc = "Previous Tab" })

-- Coding utility
vim.keymap.set('n', '<leader>cs', ":terminal<CR>", { desc = "Open terminal" })
vim.keymap.set("n", "<leader>cm", ":%s//gc<Left><Left><Left>") -- attempt at native multiple cursors
vim.keymap.set("v", "<leader>cm", ":s//gc<Left><Left><Left>")  -- attempt at native multiple cursors visual mode
vim.keymap.set("n", "<leader>*", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
vim.keymap.set("n", "<leader>X", "<cmd>!chmod +x %<CR>", { silent = true })
vim.keymap.set("n", "<leader>uc", ":! ", { silent = true })
vim.keymap.set("n", "<leader><leader>k", "<cmd>%bd|e#<cr>", { desc = "Close all buffers but the current one" }) -- https://stackoverflow.com/a/42071865/516188

-- Git
vim.keymap.set('n', '<leader>gg', vim.cmd.Git, { desc = "Launch Fugitive" })
vim.keymap.set('n', '<leader>gs', ":Git ", { desc = "Git commands" })
vim.keymap.set('n', '<leader>gm', ":Gdiffsplit!<CR>", { desc = "Open 3 way split" })
vim.keymap.set('n', '<leader>gj', ":diffget //3<CR>", { desc = "diff get 3" })
vim.keymap.set('n', '<leader>gf', ":diffget //2<CR>", { desc = "diff get 2" })
