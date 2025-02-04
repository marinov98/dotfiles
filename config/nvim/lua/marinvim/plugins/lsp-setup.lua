return {
  {
    "williamboman/mason.nvim",
    cmd = { 'Mason', 'MasonInstall', 'MasonUninstall' },
    config = function()
      require("mason").setup()
    end
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls" } -- examples: "pyright", "cssls", "ts_ls", "jsonls"
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = { 'saghen/blink.cmp' },
    event = { 'BufReadPre', 'BufNewFile' },
    config = function()
      local lspconfig = require('lspconfig')
      local capabilities = require('blink.cmp').get_lsp_capabilities()

      -- Servers
      lspconfig.lua_ls.setup({ capabilities = capabilities })
      lspconfig.pyright.setup({ capabilities = capabilities })
      lspconfig.ts_ls.setup({ capabilities = capabilities })
      lspconfig.jsonls.setup({ capabilities = capabilities })
      lspconfig.yamlls.setup({ capabilities = capabilities })
      lspconfig.html.setup({ capabilities = capabilities })
      lspconfig.cssls.setup({ capabilities = capabilities })
      lspconfig.gopls.setup({ capabilities = capabilities })
      lspconfig.rust_analyzer.setup({ capabilities = capabilities })
      lspconfig.elixirls.setup({ capabilities = capabilities })


      -- Bindings
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Next diagnostic in current file" })
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Previous diagnostic in current file" })

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Buffer local mappings.
          -- See `:help vim.lsp.*` for documentation on any of the below functions
          local opts = { buffer = ev.buf }

          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', '<leader>lg', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', '<leader>ll', function()
            vim.lsp.buf.format { async = true }
          end, opts)

          -- Goto
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', 'gy', vim.lsp.buf.type_definition, opts)
          -- Workspace related
          vim.keymap.set('n', '<leader>lws', vim.lsp.buf.workspace_symbol, opts)
          vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, opts)
          vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, opts)
          vim.keymap.set('n', '<leader>lwl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
          end, opts)

          -- Telescope Variant
          -- local builtin = require('telescope.builtin')
          -- vim.keymap.set('n', 'gd', builtin.lsp_definitions, opts)
          -- vim.keymap.set('n', 'grr', builtin.lsp_references, opts) -- better visuals than vim.lsp.buf.references
          -- vim.keymap.set('n', 'gri', builtin.lsp_implementations, opts)
          -- vim.keymap.set('n', 'gy', builtin.lsp_type_definitions, opts)
          -- vim.keymap.set('n', '<leader>ls', builtin.lsp_document_symbols, opts)
          -- vim.keymap.set('n', '<leader>lws', builtin.lsp_workspace_symbols, opts)
        end,
      })
    end
  }
}
