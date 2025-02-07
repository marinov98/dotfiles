vim.g.mapleader = " "

-- Saving, Quitting, Navigating
vim.keymap.set('n', '<leader>a', "<C-^>", { desc = "Alternate buffers" })
vim.keymap.set('n', '<leader>s', ":w<CR>", { desc = "Save File" })
vim.keymap.set('n', '<leader>k', ":bd<CR>", { desc = "Close buffer" })
vim.keymap.set("n", "<leader><leader>k", "<cmd>%bd|e#<cr>", { desc = "Close all buffers but the current one" }) -- https://stackoverflow.com/a/42071865/516188
vim.keymap.set('n', '<leader>x', ":tabclose<CR>", { desc = "Close tab" })
vim.keymap.set('n', '<leader>q', ":q<CR>", { desc = "Quit" })
vim.keymap.set('n', '<leader>Q', ":q!<CR>", { desc = "Force Quit" })

 -- Copying, Pasting
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]], { desc = "Copy to system clipboard" })
vim.keymap.set("n", "<leader>Y", [["+Y]], { desc = "Copy line to system clipboard" })
vim.keymap.set({ "n", "v" }, "<leader>p", [["+p]], { desc = "Paste from system clipboard" })
vim.keymap.set("x", "<leader>P", [["_dP]], { desc = "Paste without cutting" })

-- Coding utility
vim.keymap.set('n', '<leader>cs', ":terminal<CR>", { desc = "Open terminal" })
vim.keymap.set('n', '<leader>cn', ":noh<CR>", { desc = "Remove highlight" })
vim.keymap.set("n", "<leader>cm", ":%s//gc<Left><Left><Left>", { desc = "Search/replace in current file with confirmation" }) -- attempt at native multiple cursors
vim.keymap.set("v", "<leader>cm", ":s//gc<Left><Left><Left>", { desc = "Search/replace in visual highlight with confirmation" })  -- attempt at native multiple cursors visual mode
vim.keymap.set("n", "<leader>ci", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], { desc = "Case insensitive search/replace under cursor for current file" })

-- General Utility
vim.keymap.set("n", "<leader>ux", "<cmd>!chmod +x %<CR>", { silent = true, desc = "Chmod +x current file" })
vim.keymap.set("n", "<leader>uc", ":! ", { silent = true, desc = "Execute external command" })

-- File tree 
-- vim.keymap.set('n', '-', vim.cmd.Explore, { desc = "Open file browser" }) -- Uncomment when not using a different file tree plugin
-- vim.keymap.set('n', '<leader>tn', ":Vexplore!<CR>", { desc = "Open netrw side bar" }) -- same as above but open as a sidebar
