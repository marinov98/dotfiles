return {
  {
    'echasnovski/mini.pairs',
    version = "*",
    event = "InsertEnter",
    opts = {}
  },
  -- icons
  {
    'echasnovski/mini.icons',
    version = '*',
    opts = {}
  },
  {
    'stevearc/oil.nvim',
    cmd = "Oil",
    keys = "-",
    version = "*",
    config = function()
      require("oil").setup({
        view_options = {
          show_hidden = true
        }
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory with Oil" })
    end
  }
}
