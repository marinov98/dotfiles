return {
  "saghen/blink.cmp",
  event = { 'BufReadPre', 'BufNewFile' },
  version = "v0.*",
  dependencies = {
    "L3MON4D3/LuaSnip",             -- snippet engine
    "rafamadriz/friendly-snippets", -- useful snippets
  },
  opts = {
    signature = { enabled = true },
    keymap = {
      preset = 'default',
      ['<Tab>'] = { 'select_and_accept' },
      ['<C-y>'] = { 'snippet_forward', 'fallback' }
    }
  }
}
