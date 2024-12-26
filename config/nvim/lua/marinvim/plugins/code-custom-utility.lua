return {
  {
    'echasnovski/mini.pairs',
    version = "*",
    event = "InsertEnter",
    opts = {}
  },
  -- icons
  {
    'nvim-tree/nvim-web-devicons',
    version = false,
    opts = {}
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
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
  },
  {
    'stevearc/oil.nvim',
    version = "*",
    config = function()
      -- ensure netrw is disabled
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1

      require("oil").setup({
        view_options = {
          show_hidden = true
        }
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory with Oil" })
    end
  }
}
