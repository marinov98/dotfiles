return {
  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = {
      'mfussenegger/nvim-dap',
      "mfussenegger/nvim-dap-python",
      "nvim-neotest/nvim-nio",
      'rcarriga/nvim-dap-ui',
    },
    opts = {
      ensure_installed = {
        "codelldb"
      }
    },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      local dap_python = require("dap-python")

      dapui.setup()
      dap_python.setup("uv")

      -- Automatically open/close DAP UI
      dap.listeners.after.event_initialized['dapui_config'] = dapui.open
      dap.listeners.before.event_terminated['dapui_config'] = dapui.close
      dap.listeners.before.event_exited['dapui_config'] = dapui.close

      vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Debug: Toggle Breakpoint" })
      vim.keymap.set("n", "<leader>dB", dap.set_breakpoint, { desc = "Debug: Set Breakpoint" })

      vim.keymap.set("n", "<F5>", dap.continue, { desc = "Debug: Start/Continue" })
      vim.keymap.set("n", "<F1>", dap.step_into, { desc = "Debug: Step Into" })
      vim.keymap.set("n", "<F2>", dap.step_over, { desc = "Debug: Step Over" })
      vim.keymap.set("n", "<F3>", dap.step_out, { desc = "Debug: Step Out" })
      vim.keymap.set("n", "<F7>", dapui.toggle, { desc = "Debug: See last session" })
    end,
  }
}
