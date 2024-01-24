return {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    -- Autocompletion
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp-signature-help', -- signature help while typing
    "hrsh7th/cmp-buffer",                  -- source for text in buffer
    "hrsh7th/cmp-path",                    -- source for file system paths
    -- Snippets
    "L3MON4D3/LuaSnip",                    -- snippet engine
    "saadparwaiz1/cmp_luasnip",            -- for autocompletion
    "rafamadriz/friendly-snippets",        -- useful snippets
  },
  config = function()
    local cmp = require("cmp")
    local cmp_select = { behavior = cmp.SelectBehavior.Select } -- don't insert candidates just select them

    require("luasnip.loaders.from_vscode").lazy_load()

    cmp.setup({
      completion = {
        completeopt = "menu,menuone,preview,noselect",
        keyword_length = 2 -- show completions after X characters
      },

      formatting = {
        fields = { 'abbr', 'kind', 'menu' },
        format = function(entry, item)
          local menu_icon = {
            nvim_lsp = '[LSP]',
            luasnip = '[Lua]',
            nvim_lua = '[Nvim]',
            buffer = '[Buffer]',
            path = '[Path]',
          }

          item.menu = menu_icon[entry.source.name]
          return item
        end,
      },

      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },

      snippet = { -- configure how nvim-cmp interacts with snippet engine
        expand = function(args)
          require("luasnip").lsp_expand(args.body)
        end,
      },

      mapping = cmp.mapping.preset.insert({
        ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select), -- previous suggestion
        ["<C-n>"] = cmp.mapping.select_next_item(cmp_select), -- next suggestion
        ["<tab>"] = cmp.mapping.confirm({ select = true }),
        ["<C-Space>"] = cmp.mapping.complete(),               -- show completion suggestions
      }),

      -- sources for autocompletion
      sources = cmp.config.sources({
        { name = 'nvim_lua' },
        { name = "nvim_lsp" },
        { name = 'nvim_lsp_signature_help' },
        { name = "luasnip" },       -- snippets
        { name = "path" },          -- file system paths
        {
          name = "buffer",          -- text within current buffer
          option = {
            get_bufnrs = function() -- don't give source if file > 1 MB
              local buf = vim.api.nvim_get_current_buf()
              local byte_size = vim.api.nvim_buf_get_offset(buf, vim.api.nvim_buf_line_count(buf))
              if byte_size > 1024 * 1024 then -- 1 Megabyte max
                return {}
              end
              return { buf }
            end
          }
        },
      }),
    })
  end,
}
