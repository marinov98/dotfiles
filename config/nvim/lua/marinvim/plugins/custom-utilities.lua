return {
  {
    "kylechui/nvim-surround",
    keys = { 'cs', 'ds', 'ys' },
    version = "*",
    config = true,
  },
  {
    'echasnovski/mini.pairs',
    version = "*",
    event = "InsertEnter",
    opts = {}
  },
  -- icons
  {
    'echasnovski/mini.icons',
    enabled = false, -- set to true and enable in fuzzy finder for icons
    version = '*',
    opts = {}
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
