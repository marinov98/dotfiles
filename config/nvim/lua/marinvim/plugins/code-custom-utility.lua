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
    "nvim-tree/nvim-tree.lua",
    version = "*",
    keys = "<leader>ut",
    cmd = { "NvimTreeToggle", "NvimTreeFocus", "NvimTreeFindFile" },
    config = function()
      require("nvim-tree").setup({
        view = {
          width = 50
        }
      })
      vim.keymap.set('n', '<leader>ut', ":NvimTreeToggle<CR>", { desc = "Open File Tree" })
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
