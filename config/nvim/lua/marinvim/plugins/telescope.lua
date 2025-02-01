return {
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
      "nvim-telescope/telescope-ui-select.nvim",
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
          ["ui-select"] = {
            require("telescope.themes").get_dropdown({})
          },
          fzf = {}
        },
        pickers = {
          live_grep = {
            additional_args = function()
              return { '--hidden', '--glob', '!**/.git/*' }
            end
          },
          grep_string = {
            additional_args = function()
              return { '--hidden', '--glob', '!**/.git/*' }
            end
          }
        }

      })

      -- Extensions
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")

      -- Keybindings
      local builtin = require("telescope.builtin")

      -- Essentials
      local target_find_command = { 'rg', '--files', "--hidden", "--color", "never", "--glob=!**/.git/*" }
      local target_desc = "Fuzzy find files in current working directory (Ripgrep)"

      if vim.fn.executable('fd') == 1 then
        target_find_command = { 'fd', '--type', 'f', "--color=never", '--hidden', '--exclude', ".git" } -- might be fdfind for Ubuntu or other linux distros
        target_desc = "Fuzzy find files in current working directory (Fd)"
      end

      vim.keymap.set('n', '<leader>f', function()
        builtin.find_files({ find_command = target_find_command })
      end, { desc = target_desc })

      vim.keymap.set('n', '<leader>um', function()
        builtin.find_files({ cwd = vim.fn.stdpath('config') })
      end, { desc = "Fuzzy Find in Nvim configuration" })

      vim.keymap.set('n', '<leader>/', builtin.live_grep, { desc = "Live grep project" })
      vim.keymap.set('n', '<leader>*', builtin.grep_string, { desc = "Live grep project under cursor" })
      vim.keymap.set('n', '<leader>ur', builtin.oldfiles, { desc = "List old files" })
      vim.keymap.set('n', '<leader>bi', builtin.buffers, { desc = "List buffers" })
      vim.keymap.set('n', '<leader>ld', builtin.diagnostics, { desc = "List diagnostics in project" })
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
  }
}
