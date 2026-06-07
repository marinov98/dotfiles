return {
  {
    "folke/snacks.nvim",
    lazy = false,
    priority = 1001,
    config = function()
      local Snacks = require("snacks")

      local ivy_layout = { preset = "ivy", layout = { height = 0.3 } }

      Snacks.setup({
        bigfile = {
          enabled = true,
          max_size = 2 * 1024 * 1024, -- 2MB
        },
        image = {
          enabled = true,
        },
        picker = {
          enabled = true,
          hidden = true,
          layout = {
            layout = {
              backdrop = false,
              height = 0.5,
            },
          },
          sources = {
            projects = {
              filter = {
                paths = { ["~/projects"] = true },
              },
            },
            buffers = {
              layout = ivy_layout,
            },
            diagnostics = {
              layout = ivy_layout,
            },
            lines = {
              layout = ivy_layout
            },
          },
        },
      })

      local map = vim.keymap.set
      -- Finding, Listing
      map("n", "<leader>f", function()
        Snacks.picker.smart({
          multi = { "buffers", "recent", "files" },
          format = "file",
          matcher = {
            fuzzy = true,
            filename_bonus = true,
            history_bonus = true,
            sort_empty = true,
            frecency = true,
          },
          filter = {
            cwd = true,
          },
        })
      end, { desc = "Find (Smart) Files" })
      map("n", "<C-p>", function() Snacks.picker.files({ cmd = "fd", hidden = false }) end,
        { desc = "Find (Project) Files" })
      map("n", "gf", function()
          local file_with_suffix = vim.fn.expand("<cWORD>")
          local file = vim.fn.expand("<cfile>")
          vim.api.nvim_command("wincmd k")
          if vim.uv.fs_stat(file) then
            vim.api.nvim_command(string.format("e %s", file_with_suffix))
          else
            Snacks.picker.files({ search = file_with_suffix })
          end
        end,
        { desc = "Find File under cursor" })
      map("n", "<leader>um", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end,
        { desc = "Find Config File" })
      map("n", "<leader>ud", function() Snacks.picker.files({ cwd = "~/.config" }) end, { desc = "Find Dotfiles" })
      map("n", "<leader>bl", function() Snacks.picker.buffers() end, { desc = "List Buffers" })
      map("n", "<leader>bt", function() Snacks.picker.treesitter() end, { desc = "Buffer Treesitter Symbols" })
      map("n", "<leader>dl", function() Snacks.picker.diagnostics() end, { desc = "List Diagnostics" })
      map("n", "<leader><leader>l", function() Snacks.picker.projects() end, { desc = "List Projects" })
      -- Grep
      map("n", "<leader>/", function() Snacks.picker.grep_word({ search = vim.fn.input("Grep > ") }) end,
        { desc = "Grep on user input" })
      map({ "n", "x" }, "<leader>*", function() Snacks.picker.grep_word() end, { desc = "Grep word under cursor/visual" })
      map("n", "<leader><leader>*", function() Snacks.picker.grep_word({ search = vim.fn.expand("<cWORD>") }) end,
        { desc = "Grep WORD under cursor" })
      -- Git
      map("n", "<leader><leader>f", function() Snacks.picker.git_files() end, { desc = "Find Git Files" })
      map("n", "<leader>gc", function() Snacks.picker.git_log() end, { desc = "Git Commits(Log)" })
      map("n", "<leader>gb", function() Snacks.picker.git_branches() end, { desc = "Git Branches" })
      map("n", "<leader>gt", function() Snacks.picker.git_status() end, { desc = "Git Status" })
      map("n", "<leader>gh", function() Snacks.picker.git_stash() end, { desc = "Git Stash" })
      -- File Tree
      map("n", "<leader>ut", function() Snacks.picker.explorer() end, { desc = "File Tree Explorer" })
      -- Help
      map("n", "<C-h>", function() Snacks.picker.help() end, { desc = "Show Help Tags" })
      map("n", "<leader>uk", function() Snacks.picker.keymaps() end, { desc = "Show Keymaps" })
      map("n", "<leader>?c", function() Snacks.picker.commands() end, { desc = "Show Commands" })
      map("n", "<leader>?h", function() Snacks.picker.command_history() end, { desc = "Show Command History" })
      map("n", "<leader>?m", function() Snacks.picker.man() end, { desc = "Show Man Pages" })
      map("n", "<leader>?j", function() Snacks.picker.jumps() end, { desc = "Show Jump Lists" })
      map("n", "<leader>?l", function() Snacks.picker.loclist() end, { desc = "Show Location List" })
      map("n", "<leader>?s", function() Snacks.picker.search_history() end, { desc = "Show Search History" })
      map("n", "<leader>?r", function() Snacks.picker.registers() end, { desc = "Show Registers" })
      map("n", "<leader>?M", function() Snacks.picker.marks() end, { desc = "Show Marks" })
      map("n", "<leader>?t", function() Snacks.picker.colorschemes() end, { desc = "Show Themes" })
    end,
  }
}
