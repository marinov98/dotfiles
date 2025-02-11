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
      { "<leader>f",         function() Snacks.picker.files({ cmd = "fd" }) end,                         desc = "Find Files" },
      { "<leader>um",        function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end,     desc = "Find Config File" },
      { "<leader>ud",        function() Snacks.picker.files({ cwd = "~/.config" }) end,                  desc = "Find Dotfiles" },
      { "<leader>bl",        function() Snacks.picker.buffers() end,                                     desc = "List Buffers" },
      { "<leader>bt",        function() Snacks.picker.treesitter() end,                                  desc = "Buffer Treesitter Symbols" },
      { "<leader>dl",        function() Snacks.picker.diagnostics() end,                                 desc = "List Diagnostics" },
      { "<leader><leader>l", function() Snacks.picker.projects() end,                                    desc = "Find Projects" },
      -- Grep
      { "<leader>/",         function() Snacks.picker.grep() end,                                        desc = "Grep Current Working Directory" },
      { "<leader>*",         function() Snacks.picker.grep_word() end,                                   desc = "Grep Current Working Directory under cursor" },
      { "<leader><leader>g", function() Snacks.picker.grep_word({ search = vim.fn.input("rg > ") }) end, desc = "Grep on user input" },
      -- Git
      { "<leader><leader>f", function() Snacks.picker.git_files() end,                                   desc = "Find Git Files" },
      { "<leader>gc",        function() Snacks.picker.git_log() end,                                     desc = "Git Commits(Log)" },
      { "<leader>gb",        function() Snacks.picker.git_branches() end,                                desc = "Git Branches" },
      { "<leader>gt",        function() Snacks.picker.git_status() end,                                  desc = "Git Status" },
      { "<leader>gh",        function() Snacks.picker.git_stash() end,                                   desc = "Git Stash" },
      -- File Tree
      { "<leader>tn",        function() Snacks.picker.explorer() end,                                    desc = "File Explorer" },
      -- Help
      { "<leader>?t",        function() Snacks.picker.help() end,                                        desc = "Show Help Tags" },
      { "<leader>?k",        function() Snacks.picker.keymaps() end,                                     desc = "Show Keymaps" },
      { "<leader>?c",        function() Snacks.picker.commands() end,                                    desc = "Show Commands" },
      { "<leader>?h",        function() Snacks.picker.command_history() end,                             desc = "Show Command History" },
      { "<leader>?m",        function() Snacks.picker.man() end,                                         desc = "Show Man Pages" },
      { "<leader>?j",        function() Snacks.picker.jumps() end,                                       desc = "Show Jump Lists" },
      { "<leader>?l",        function() Snacks.picker.loclist() end,                                     desc = "Show Location List" },
      { "<leader>?s",        function() Snacks.picker.search_history() end,                              desc = "Show Search History" },
      { "<leader>?r",        function() Snacks.picker.registers() end,                                   desc = "Show Registers" },
      { "<leader>?M",        function() Snacks.picker.marks() end,                                       desc = "Show Marks" },
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

      vim.keymap.set('n', '<leader>f', function() picker.files() end,
        { desc = "Fuzzy find files in current working directory" })

      vim.keymap.set('n', '<leader>um', function() picker.files({ cwd = vim.fn.stdpath('config') }) end,
        { desc = "Fuzzy Find in Nvim configuration" })

      vim.keymap.set('n', '<leader>ud', function() picker.files({ cwd = '~/.config' }) end,
        { desc = "Find Dotfiles" })

      vim.keymap.set('n', '<leader>bl', picker.buffers, { desc = "List buffers" })
      vim.keymap.set('n', '<leader>bt', picker.treesitter, { desc = "Buffer treesitter" })
      vim.keymap.set('n', '<leader>dl', picker.diagnostics_workspace, { desc = "List diagnostics" })

      vim.keymap.set('n', '<leader>/', picker.live_grep_native, { desc = "Grep Current Working Directory" })
      vim.keymap.set('n', '<leader>*', picker.grep_cword, { desc = "Live grep project under cursor" })
      vim.keymap.set('n', '<leader><leader>g', picker.grep, { desc = "Grep on user input" })

      vim.keymap.set('n', '<leader><leader>f', picker.git_files, { desc = "Find Git Files" })
      vim.keymap.set('n', '<leader>gc', picker.git_commits, { desc = "List git commits" })
      vim.keymap.set('n', '<leader>gC', picker.git_bcommits, { desc = "List git buffer commits" })
      vim.keymap.set('n', '<leader>gb', picker.git_branches, { desc = "List git branches" })
      vim.keymap.set('n', '<leader>gt', picker.git_status, { desc = "Show git status" })
      vim.keymap.set('n', '<leader>gh', picker.git_stash, { desc = "Git stash" })

      -- Helpful
      vim.keymap.set('n', '<leader>?t', picker.helptags, { desc = "Show help tags" })
      vim.keymap.set('n', '<leader>?b', picker.builtin, { desc = "Show telescope builtins" })
      vim.keymap.set('n', '<leader>?k', picker.keymaps, { desc = "Show keymaps" })
      vim.keymap.set('n', '<leader>?c', picker.commands, { desc = "List commands" })
      vim.keymap.set('n', '<leader>?h', picker.command_history, { desc = "Show command history" })
      vim.keymap.set('n', '<leader>?m', picker.man_pages, { desc = "List man pages" })
      vim.keymap.set('n', '<leader>?j', picker.jumps, { desc = "List jumplists" })
      vim.keymap.set('n', '<leader>?l', picker.loclist, { desc = "Show loclist" })
      vim.keymap.set('n', '<leader>?s', picker.search_history, { desc = "Show search history" })
      vim.keymap.set('n', '<leader>?r', picker.registers, { desc = "List registers" })
    end
  }
}
