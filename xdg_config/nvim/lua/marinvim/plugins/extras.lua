return {
  {
    "kylechui/nvim-surround",
    keys = { 'cs', 'ds', 'ys' },
    opts = {}
  },
  {
    'echasnovski/mini.pairs',
    enabled = false,
    event = "InsertEnter",
    opts = {}
  },
  {
    'echasnovski/mini.icons',
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
    opts = { view_options = { show_hidden = true } },
    cmd = "Oil",
    keys = {
      { "-", "<CMD>Oil<CR>", desc = "Open parent directory with Oil" }
    },
  },
  {
    'MagicDuck/grug-far.nvim',
    opts = {},
    cmd = { "GrugFar", "GrugFarWithin" },
    keys = {
      {
        "<leader><leader>/",
        function()
          require("grug-far").open({ visualSelectionUsage = 'operate-within-range' })
        end,
        mode = { 'n', 'x' },
        desc = 'Search within range'
      },
      {
        "<leader><leader>*",
        function()
          require("grug-far").with_visual_selection()
        end,
        mode = "x",
        desc = "Search/Replace Visual Selection"
      }
    },
  }
}
