return {
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "G", "Gdiffsplit" },
    keys = { '<leader>gg', '<leader>gs', '<leader>gm', '<leader>gdj', '<leader>gdk' },
    config = function()
      vim.keymap.set('n', '<leader>gg', vim.cmd.Git, { desc = "Launch Fugitive" })
      vim.keymap.set('n', '<leader>gs', ":Git ", { desc = "Git commands" })
      vim.keymap.set('n', '<leader>gm', ":Gdiffsplit!<CR>", { desc = "Open 3 way split" })
      vim.keymap.set('n', '<leader>gdj', ":diffget //2<CR>", { desc = "diff get 2" })
      vim.keymap.set('n', '<leader>gdk', ":diffget //3<CR>", { desc = "diff get 3" })
    end
  }
}
