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
      lspconfig.lua_ls.setup({
        capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = { "vim", "it", "describe", "Snacks" }
            }
          }
        }
      })

      lspconfig.pyright.setup({ capabilities = capabilities })
      lspconfig.ts_ls.setup({ capabilities = capabilities })
      lspconfig.jsonls.setup({ capabilities = capabilities })
      lspconfig.yamlls.setup({ capabilities = capabilities })
      lspconfig.html.setup({ capabilities = capabilities })
      lspconfig.cssls.setup({ capabilities = capabilities })
      lspconfig.gopls.setup({ capabilities = capabilities })
      lspconfig.rust_analyzer.setup({ capabilities = capabilities })
      lspconfig.elixirls.setup({ capabilities = capabilities })

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          local local_buf = ev.buf

          -- Diagnostic
          vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Next diagnostic in current file" })
          vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Previous diagnostic in current file" })
          vim.keymap.set('n', '<leader>dg', vim.diagnostic.open_float, { desc = "Glance Diagnostic" })

          -- L (lsp)
          vim.keymap.set('n', '<leader>lg', vim.lsp.buf.hover, { desc = "LSP Glance", buffer = local_buf })
          vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, { desc = "LSP Code Action", buffer = local_buf })
          vim.keymap.set('n', '<leader>lF', function()
              vim.lsp.buf.format { async = true }
          end, { desc = "LSP format file", buffer = local_buf })

          vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, { desc = "Add workspace folder", buffer = local_buf })
          vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, { desc = "Remove workspace folder", buffer = local_buf })

          -- Original
          vim.keymap.set('n', 'grd', vim.lsp.buf.declaration, { desc = "Go to declaration", buffer = local_buf })
          vim.keymap.set('n', 'grn', vim.lsp.buf.rename, { desc = "Rename variable", buffer = local_buf })
          vim.keymap.set('i', '<C-h>', vim.lsp.buf.signature_help, { desc = "Signature Help", buffer = local_buf })
          -- vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { desc = "Show Definitions", buffer = local_buf })
          -- vim.keymap.set('n', 'grr', vim.lsp.buf.references, { desc = "Show References", buffer = local_buf })
          -- vim.keymap.set('n', 'gri', vim.lsp.buf.implementation, { desc = "Show implementations", buffer = local_buf })
          -- vim.keymap.set('n', 'gry', vim.lsp.buf.type_definition, { desc = "Show Type Definitions", buffer = local_buf })
          -- vim.keymap.set('n', '<leader>ls', vim.lsp.buf.document_symbol, { desc = "Document Symbol", buffer = local_buf })
          -- vim.keymap.set('n', '<leader>lws', vim.lsp.buf.workspace_symbol, { desc = "Workspace Symbols", buffer = local_buf })

          -- Picker Variant
          local picker = Snacks.picker
          vim.keymap.set('n', 'gd', function() picker.lsp_definitions() end, { desc = "Show Definitions", buffer = local_buf })
          vim.keymap.set('n', 'grr', function() picker.lsp_references() end, { desc = "Show References", buffer = local_buf })
          vim.keymap.set('n', 'gri', function() picker.lsp_implementations() end, { desc = "Show implementations", buffer = local_buf })
          vim.keymap.set('n', 'gry', function() picker.lsp_type_definitions() end, { desc = "Show Type Definitions", buffer = local_buf })
          vim.keymap.set('n', '<leader>ls', function() picker.lsp_symbols() end, { desc = "Document Symbol", buffer = local_buf })
          vim.keymap.set('n', '<leader>lws', function() picker.lsp_workspace_symbols() end, { desc = "Workspace Symbols", buffer = local_buf })
        end,
      })
    end
  }
}
