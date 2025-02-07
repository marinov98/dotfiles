return {
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    },
    config = function()
      -- Setup
      local telescope = require("telescope")
      telescope.setup({
        defaults = {
          layout_strategy = "horizontal",
          layout_config = {
            horizontal = {
              prompt_position = "top",
            },
          },
          sorting_strategy = "ascending"
        },
        extensions = {
          fzf = {}
        },
        pickers = {
          buffers = {
            theme = 'ivy'
          },
          diagnostics = {
            theme = 'ivy'
          },
          live_grep = {
            additional_args = { "--hidden" }
          },
          grep_string = {
            additional_args = { "--hidden" }
          }
        }

      })

      -- Extensions
      telescope.load_extension("fzf")

      -- Keybindings
      local builtin = require("telescope.builtin")

      -- Essentials
      local ff_command = { 'rg', '--files', "--hidden", "--color", "never" }

      -- priority for find_files should be fd if you ask me
      if vim.fn.executable('fd') == 1 then
        ff_command = { 'fd', '--type', 'f', "--color=never", '--hidden' }
      elseif vim.fn.executable('fdfind') == 1 then
        ff_command = { 'fdfind', '--type', 'f', "--color=never", '--hidden' }
      end

      vim.keymap.set('n', '<leader>f', function() builtin.find_files({ find_command = ff_command }) end,
        { desc = "Fuzzy find files in current working directory" })

      vim.keymap.set('n', '<leader>um', function() builtin.find_files({ cwd = vim.fn.stdpath('config') }) end,
        { desc = "Fuzzy Find in Nvim configuration" })

      vim.keymap.set('n', '<leader>/', builtin.live_grep, { desc = "Live grep project" })
      vim.keymap.set('n', '<leader>*', builtin.grep_string, { desc = "Live grep project under cursor" })
      vim.keymap.set('n', '<leader>bl', builtin.buffers, { desc = "List buffers" })
      vim.keymap.set('n', '<leader>dl', builtin.diagnostics, { desc = "List diagnostics" })
      -- Project Specific
      vim.keymap.set('n', '<leader><leader>f', builtin.git_files, { desc = "Find git files" })
      vim.keymap.set('n', '<leader><leader>g', function()
        builtin.grep_string({ search = vim.fn.input("Grep > ") })
      end, { desc = "Live grep on user input" })
      -- Git specific
      vim.keymap.set('n', '<leader>gc', builtin.git_commits, { desc = "List git commits" })
      vim.keymap.set('n', '<leader>gC', builtin.git_bcommits, { desc = "List git bcommits" })
      vim.keymap.set('n', '<leader>gb', builtin.git_branches, { desc = "List git branches" })
      vim.keymap.set('n', '<leader>gt', builtin.git_status, { desc = "Show git status" })
      vim.keymap.set('n', '<leader>gh', builtin.git_stash, { desc = "Git stash" })
      -- Helpful
      vim.keymap.set('n', '<leader>?t', builtin.help_tags, { desc = "Show help tags" })
      vim.keymap.set('n', '<leader>?b', builtin.builtin, { desc = "Show telescope builtins" })
      vim.keymap.set('n', '<leader>?k', builtin.keymaps, { desc = "Show keymaps" })
      vim.keymap.set('n', '<leader>?c', builtin.commands, { desc = "List commands" })
      vim.keymap.set('n', '<leader>?h', builtin.command_history, { desc = "Show command history" })
      vim.keymap.set('n', '<leader>?m', builtin.man_pages, { desc = "List man pages" })
      vim.keymap.set('n', '<leader>?j', builtin.jumplist, { desc = "List jumplists" })
      vim.keymap.set('n', '<leader>?l', builtin.loclist, { desc = "Show loclist" })
      vim.keymap.set('n', '<leader>?s', builtin.search_history, { desc = "Show search history" })
      vim.keymap.set('n', '<leader>?r', builtin.registers, { desc = "List registers" })
    end
  },
  {
    "folke/snacks.nvim",
    lazy = false,
    priority = 999,
    opts = {
      picker = {
        enabled = true,
        layout = {
          layout = {
            backdrop = false
          }
        },
        sources = {
          files = {
            hidden = true
          },
          buffers = {
            layout = { preset = "ivy" },
          },
          diagnostics = {
            layout = { preset = "ivy" },
          },
          grep = { hidden = true }
        },
        icons = { files = { enabled = false } }
      }
    },
    keys = {
      -- Top Pickers & Explorer
      { "<leader><leader>G", function() Snacks.picker.grep() end,                                       desc = "Grep" },
      { "<leader>e",         function() Snacks.picker.grep_word({ search = vim.fn.input("rg >") }) end, desc = "File Explorer" },
      { "<leader>E",         function() Snacks.picker.lsp_references() end,                             desc = "File Explorer" },
      -- find
      { "<leader>z",         function() Snacks.picker.files() end,                                      desc = "Find Files" } }
  },
}
