return {
  {
    "folke/snacks.nvim",
    lazy = false,
    enabled = false,
    priority = 1001,
    opts = {
      picker = {
        enabled = true,
        layout = {
          layout = {
            backdrop = false
          }
        },
        sources = {
          projects = {
            filter = {
              paths = { ["~/projects"] = true },
            }
          },
          files = {
            hidden = true
          },
          buffers = {
            layout = { preset = "ivy" },
          },
          diagnostics = {
            layout = { preset = "ivy" },
          },
          grep = { hidden = true },
          grep_word = { hidden = true }
        },
        icons = { files = { enabled = false } }
      }
    },
    keys = {
      -- Finding
      -- { "<leader>f",         function() Snacks.picker.files({ cmd = "rg" }) end,                         desc = "Find Files" },
      { "<leader>f",         function() Snacks.picker.files({ cmd = "fd" }) end,                           desc = "Find Files" },
      { "<leader>um",        function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end,       desc = "Find Config File" },
      { "<leader>ud",        function() Snacks.picker.files({ cwd = "~/.config" }) end,                    desc = "Find Dotfiles" },
      { "<leader>bl",        function() Snacks.picker.buffers() end,                                       desc = "List Buffers" },
      { "<leader>bt",        function() Snacks.picker.treesitter() end,                                    desc = "Buffer Treesitter Symbols" },
      { "<leader>dl",        function() Snacks.picker.diagnostics() end,                                   desc = "List Diagnostics" },
      { "<leader><leader>l", function() Snacks.picker.projects() end,                                      desc = "Find Projects" },
      -- Grep
      { "<leader>/",         function() Snacks.picker.grep() end,                                          desc = "Grep Current Working Directory" },
      { "<leader>*",         function() Snacks.picker.grep_word() end,                                     desc = "Grep Current Working Directory under cursor" },
      { "<leader><leader>g", function() Snacks.picker.grep_word({ search = vim.fn.input("Grep > ") }) end, desc = "Grep on user input" },
      -- Git
      { "<leader><leader>f", function() Snacks.picker.git_files() end,                                     desc = "Find Git Files" },
      { "<leader>gc",        function() Snacks.picker.git_log() end,                                       desc = "Git Commits(Log)" },
      { "<leader>gb",        function() Snacks.picker.git_branches() end,                                  desc = "Git Branches" },
      { "<leader>gt",        function() Snacks.picker.git_status() end,                                    desc = "Git Status" },
      { "<leader>gh",        function() Snacks.picker.git_stash() end,                                     desc = "Git Stash" },
      -- File Tree
      { "<leader>tn",        function() Snacks.picker.explorer() end,                                      desc = "File Explorer" },
      -- Help
      { "<leader>?t",        function() Snacks.picker.help() end,                                          desc = "Show Help Tags" },
      { "<leader>?k",        function() Snacks.picker.keymaps() end,                                       desc = "Show Keymaps" },
      { "<leader>?c",        function() Snacks.picker.commands() end,                                      desc = "Show Commands" },
      { "<leader>?h",        function() Snacks.picker.command_history() end,                               desc = "Show Command History" },
      { "<leader>?m",        function() Snacks.picker.man() end,                                           desc = "Show Man Pages" },
      { "<leader>?j",        function() Snacks.picker.jumps() end,                                         desc = "Show Jump Lists" },
      { "<leader>?l",        function() Snacks.picker.loclist() end,                                       desc = "Show Location List" },
      { "<leader>?s",        function() Snacks.picker.search_history() end,                                desc = "Show Search History" },
      { "<leader>?r",        function() Snacks.picker.registers() end,                                     desc = "Show Registers" },
      { "<leader>?M",        function() Snacks.picker.marks() end,                                         desc = "Show Marks" },
    }
  },
  {
    "ibhagwan/fzf-lua",
    priority = 1001,
    lazy = false,
    config = function()
      require('fzf-lua').setup({
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept"
          }
        },
        winopts = {
          backdrop = 100
        },
        defaults = {
          hidden = true
        }
      })

      local picker = require('fzf-lua')
      picker.register_ui_select()


      -- Finding
      vim.keymap.set('n', '<leader>f', picker.files, { desc = "Find Files" })
      vim.keymap.set('n', '<leader>bl', picker.buffers, { desc = "List Buffers" })
      vim.keymap.set('n', '<leader>bt', picker.treesitter, { desc = "Buffer Treesitter Symbols" })
      vim.keymap.set('n', '<leader>dl', picker.diagnostics_workspace, { desc = "List Diagnostics" })
      -- Utility
      vim.keymap.set('n', '<leader>um', function()
        picker.files({ cwd = vim.fn.stdpath('config') })
      end, { desc = "Find Config File" })

      vim.keymap.set('n', '<leader>ud', function()
        picker.files({ cwd = '~/.config' })
      end, { desc = "Find Dotfiles" })
      -- Grep
      vim.keymap.set('n', '<leader>/', picker.live_grep_native, { desc = "Grep Current Working Directory" })
      vim.keymap.set('n', '<leader>*', picker.grep_cword, { desc = "Grep Current Working Directory under cursor" })
      vim.keymap.set('n', '<leader><leader>g', picker.grep, { desc = "Grep on user input" })
      --Git
      vim.keymap.set('n', '<leader><leader>f', picker.git_files, { desc = "Find Git Files" })
      vim.keymap.set('n', '<leader>gc', picker.git_commits, { desc = "Git Commits" })
      vim.keymap.set('n', '<leader>gC', picker.git_bcommits, { desc = "Git Buffer Commits" })
      vim.keymap.set('n', '<leader>gb', picker.git_branches, { desc = "Git Branches" })
      vim.keymap.set('n', '<leader>gt', picker.git_status, { desc = "Git Status" })
      vim.keymap.set('n', '<leader>gh', picker.git_stash, { desc = "Git Stash" })
      vim.keymap.set('n', '<leader>gl', picker.git_blame, { desc = "Git Blame Buffer" })
      -- Helpful
      vim.keymap.set('n', '<leader>?t', picker.helptags, { desc = "Show Help Tags" })
      vim.keymap.set('n', '<leader>?b', picker.builtin, { desc = "Show Builtins" })
      vim.keymap.set('n', '<leader>?k', picker.keymaps, { desc = "Show Keymaps" })
      vim.keymap.set('n', '<leader>?c', picker.commands, { desc = "Show Commands" })
      vim.keymap.set('n', '<leader>?h', picker.command_history, { desc = "Show Command History" })
      vim.keymap.set('n', '<leader>?m', picker.man_pages, { desc = "Show Man Pages" })
      vim.keymap.set('n', '<leader>?j', picker.jumps, { desc = "Show Jump Lists" })
      vim.keymap.set('n', '<leader>?l', picker.loclist, { desc = "Show Location Lists" })
      vim.keymap.set('n', '<leader>?s', picker.search_history, { desc = "Show Search History" })
      vim.keymap.set('n', '<leader>?r', picker.registers, { desc = "List Registers" })
      vim.keymap.set('n', '<leader>?M', picker.marks, { desc = "Show Marks" })
    end
  }
}
