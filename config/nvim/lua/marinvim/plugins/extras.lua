return {
  {
    "kylechui/nvim-surround",
    keys = { 'cs', 'ds', 'ys' },
    version = "*",
    config = true,
  },
  {
    'echasnovski/mini.pairs',
    enabled = false,
    version = "*",
    event = "InsertEnter",
    opts = {}
  },
  {
    'echasnovski/mini.icons',
    enabled = true,
    version = '*',
    opts = {}
  },
  {
    "j-hui/fidget.nvim",
    opts = {
      notification = {
        window = {
          winblend = 0,
        }
      }
    }
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
