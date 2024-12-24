return {
  "saghen/blink.cmp",
  event = { 'LspAttach' },
  version = "v0.*",
  dependencies = {
    "L3MON4D3/LuaSnip",             -- snippet engine
    "rafamadriz/friendly-snippets", -- useful snippets
  },
  opts = {
    signature = { enabled = true },
    keymap = {
      preset = 'super-tab',
    },
    completion = {
      list = {
        max_items = 10
      },
    },
    sources = {
      min_keyword_length = 2
    }
  }
}
