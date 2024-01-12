return {
  {
    "williamboman/mason.nvim",
    cmd = 'Mason',
    config = function()
      require("mason").setup()
    end
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls" } -- examples: "pyright", "cssls", "tsserver", "jsonls"
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local lspconfig = require('lspconfig')

      -- Servers
      lspconfig.lua_ls.setup({})
      lspconfig.pyright.setup({})
      lspconfig.tsserver.setup({})
      lspconfig.jsonls.setup({})
      lspconfig.yamlls.setup({})
      lspconfig.html.setup({})
      lspconfig.cssls.setup({})

      -- Bindings
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next)

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }
          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gh', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', '<space>lc', vim.lsp.buf.rename, opts)
          vim.keymap.set('n', '<space>la', vim.lsp.buf.code_action, opts)
          vim.keymap.set('n', '<space>lg', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', '<space>lf', function()
            vim.lsp.buf.format { async = true }
          end, opts)

          -- Enhance builtin-commands with Telescope
          local builtin = require('telescope.builtin')
          vim.keymap.set('n', 'gd', builtin.lsp_definitions, opts)
          vim.keymap.set('n', 'gr', builtin.lsp_references, opts) -- better visuals than vim.lsp.buf.references
          vim.keymap.set('n', 'gi', builtin.lsp_implementations, opts)
          vim.keymap.set('n', 'gy', builtin.lsp_type_definitions, opts)
          vim.keymap.set('n', '<space>ls', builtin.lsp_document_symbols, opts)
          -- Workspace related
          vim.keymap.set('n', '<space>lws', builtin.lsp_workspace_symbols, opts)
          vim.keymap.set('n', '<space>lwa', vim.lsp.buf.add_workspace_folder, opts)
          vim.keymap.set('n', '<space>lwr', vim.lsp.buf.remove_workspace_folder, opts)
          vim.keymap.set('n', '<space>lwl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts)

          -- old commands use if telescope is not installed or you prefer them
          -- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          -- vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
          -- vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          -- vim.keymap.set('n', 'gy', vim.lsp.buf.type_definition, opts)
        end,
      })
    end
  }
}
