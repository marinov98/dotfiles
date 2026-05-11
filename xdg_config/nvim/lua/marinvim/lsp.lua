local picker = require("snacks").picker
local lsp = vim.lsp.buf
local map = vim.keymap.set


vim.lsp.config("*", {
  capabilities = vim.tbl_deep_extend(
    "force",
    {},
    vim.lsp.protocol.make_client_capabilities(),
    require("blink.cmp").get_lsp_capabilities())
})


vim.lsp.config("lua_ls", {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
    },
  },
})


local severity = vim.diagnostic.severity
vim.diagnostic.config({
  signs = {
    text = {
      [severity.ERROR] = "",
      [severity.WARN]  = "",
      [severity.INFO]  = "",
      [severity.HINT]  = "",
    },
  },
  virtual_text = true,
  float = {
    focusable = true,
    border = "rounded",
  }
})

local function enable_document_highlight(client, buffer)
  if not client or not client:supports_method("textDocument/documentHighlight") then
    return
  end

  local group = vim.api.nvim_create_augroup("LspDocumentHighlight", { clear = true })
  vim.api.nvim_create_autocmd("CursorHold", {
    buffer = buffer,
    group = group,
    callback = vim.lsp.buf.document_highlight
  })

  vim.api.nvim_create_autocmd("CursorMoved", {
    buffer = buffer,
    group = group,
    callback = vim.lsp.buf.clear_references
  })
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(args)
    local opts = { buffer = args.buf, silent = true }

    -- Diagnostics
    map("n", "<leader>dg", vim.diagnostic.open_float, opts)

    -- LSP utilities
    map("n", "grd", lsp.declaration, opts)
    map("n", "grh", lsp.signature_help, opts)
    map("i", "<C-h>", lsp.signature_help, opts)
    map("n", "<leader>lg", lsp.hover, opts)
    map("n", "<leader>lF", function() lsp.format({ async = true }) end, opts)
    map("n", "<leader>lwa", lsp.add_workspace_folder, opts)
    map("n", "<leader>lwr", lsp.remove_workspace_folder, opts)
    map("n", "<leader>lh",
      function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled({ bufnr = opts.buffer }),
          { bufnr = opts.buffer })
      end, opts)

    -- Picker
    map("n", "gd", picker.lsp_definitions, opts)
    map("n", "grr", picker.lsp_references, opts)
    map("n", "gri", picker.lsp_implementations, opts)
    map("n", "grt", picker.lsp_type_definitions, opts)
    map("n", "<leader>ls", picker.lsp_symbols, opts)
    map("n", "<leader>lws", picker.lsp_workspace_symbols, opts)
    map("n", "<leader>lci", picker.lsp_incoming_calls, opts)
    map("n", "<leader>lco", picker.lsp_outgoing_calls, opts)

    local client = vim.lsp.get_client_by_id(args.data.client_id)
    enable_document_highlight(client, opts.buffer)
  end
})
