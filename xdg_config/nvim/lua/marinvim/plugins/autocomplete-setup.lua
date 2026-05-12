return {
  {
    "saghen/blink.cmp",
    version = "v1.*",
    dependencies = {
      "rafamadriz/friendly-snippets", -- useful snippets
      { "folke/lazydev.nvim", ft = "lua", opts = {} },
    },
    opts = {
      signature = { enabled = true, window = { border = "rounded" } },
      keymap = {
        preset = 'super-tab',
      },
      completion = {
        list = {
          max_items = 10
        },
      },
      sources = {
        min_keyword_length = 2,
        per_filetype = {
          lua = { inherit_defaults = true, 'lazydev' }
        },
        providers = {
          lazydev = {
            name = "LazyDev",
            module = "lazydev.integrations.blink",
            -- make lazydev completions top priority (see `:h blink.cmp`)
            score_offset = 100,
          },
        },
      },
    }
  }
}
