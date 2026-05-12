return {
  {
    "saghen/blink.cmp",
    version = "v1.*",
    dependencies = {
      "rafamadriz/friendly-snippets", -- useful snippets
    },
    opts = {
      signature = { enabled = true, window = { border = "rounded" } },
      snippets = { preset = "default" },
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
      },
    }
  }
}
