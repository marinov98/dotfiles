return  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      'nvim-tree/nvim-web-devicons',
      "MunifTanjim/nui.nvim",
    },
    keys = '<leader>tn',
    config = function()
      require("neo-tree").setup({
      filesystem = {
          filtered_items = {
            visible = true, -- This is what you want: If you set this to `true`, all "hide" just mean "dimmed out"
            hide_dotfiles = false,
            hide_gitignored = false,
          },
        }
      })
      vim.keymap.set('n', '<leader>tn',":Neotree toggle<CR>", {})
    end
}
