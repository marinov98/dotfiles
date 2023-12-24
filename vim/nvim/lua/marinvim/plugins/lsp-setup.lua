return {
  {
    "williamboman/mason.nvim",
    config = function()
      require("mason").setup()
    end
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls", "pyright", "cssls", "tsserver", "jsonls"}
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = require('lspconfig')

      -- Servers
      lspconfig.lua_ls.setup({})
      lspconfig.pyright.setup({})
      lspconfig.tsserver.setup({})

      -- Bindings
      vim.keymap.set('n', '<leader>lE', vim.diagnostic.goto_prev)
      vim.keymap.set('n', '<leader>le', vim.diagnostic.goto_next)

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('n', 'gy', vim.lsp.buf.type_definition, opts)
          vim.keymap.set('n', 'gh', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', '<space>lc', vim.lsp.buf.rename, opts)
          vim.keymap.set('n', '<space>la', vim.lsp.buf.code_action, opts)
          vim.keymap.set('n', '<space>lg', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', '<space>lf', function()
            vim.lsp.buf.format { async = true }
          end, opts)
      end,
      })
    end
  }
}
