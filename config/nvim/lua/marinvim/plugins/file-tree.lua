return  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      'nvim-tree/nvim-web-devicons',
      "MunifTanjim/nui.nvim",
    },
    cmd = "Neotree",
    keys = '<leader>tn',
    config = function()
      require("neo-tree").setup({
        filesystem = {
          filtered_items = {
            visible = true,
            hide_dotfiles = false,
            hide_gitignored = false,
          },
        }
      })
      vim.keymap.set('n', '<leader>tn', ":Neotree toggle<CR>", {})
    end
}
