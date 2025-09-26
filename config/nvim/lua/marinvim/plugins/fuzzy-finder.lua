return {
  {
    "folke/snacks.nvim",
    lazy = false,
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
          }
        },
        icons = { files = { enabled = false } }
      }
    },
    keys = {
      -- Finding, Listing
      { "<leader>f",         function() Snacks.picker.files({ cmd = "fd" }) end,                           desc = "Find Files" },
      { "<leader>um",        function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end,       desc = "Find Config File" },
      { "<leader>ud",        function() Snacks.picker.files({ cwd = "~/.config" }) end,                    desc = "Find Dotfiles" },
      { "<leader>bl",        function() Snacks.picker.buffers() end,                                       desc = "List Buffers" },
      { "<leader>bt",        function() Snacks.picker.treesitter() end,                                    desc = "Buffer Treesitter Symbols" },
      { "<leader>dl",        function() Snacks.picker.diagnostics() end,                                   desc = "List Diagnostics" },
      { "<leader><leader>l", function() Snacks.picker.projects() end,                                      desc = "List Projects" },
      -- Grep
      { "gl",                function() Snacks.picker.lines({ layout = { preset = "ivy" } }) end,          desc = "Goto line" },
      { "gL",                function() Snacks.picker.grep_buffers({ layout = { preset = "ivy" } }) end,   desc = "Goto line-multi" },
      { "<leader>/",         function() Snacks.picker.grep_word({ search = vim.fn.input("Grep > ") }) end, desc = "Grep on user input" },
      { "<leader><leader>/", function() Snacks.picker.grep() end,                                          desc = "Live Grep" },
      {
        "<leader>*",
        function()
          Snacks.picker.grep_word()
        end,
        mode = { "n", "x" },
        desc = "Grep word under cursor/visual"
      },
      { "<leader><leader>*", function() Snacks.picker.grep_word({ search = vim.fn.expand("<cWORD>") }) end, desc = "Grep WORD under cursor" },
      -- Git
      { "<leader><leader>f", function() Snacks.picker.git_files() end,                                      desc = "Find Git Files" },
      { "<leader>gc",        function() Snacks.picker.git_log() end,                                        desc = "Git Commits(Log)" },
      { "<leader>gb",        function() Snacks.picker.git_branches() end,                                   desc = "Git Branches" },
      { "<leader>gt",        function() Snacks.picker.git_status() end,                                     desc = "Git Status" },
      { "<leader>gh",        function() Snacks.picker.git_stash() end,                                      desc = "Git Stash" },
      -- File Tree
      { "<leader>ut",        function() Snacks.picker.explorer() end,                                       desc = "File Tree Explorer" },
      -- Help
      { "<leader>?t",        function() Snacks.picker.help() end,                                           desc = "Show Help Tags" },
      { "<leader>?k",        function() Snacks.picker.keymaps() end,                                        desc = "Show Keymaps" },
      { "<leader>?c",        function() Snacks.picker.commands() end,                                       desc = "Show Commands" },
      { "<leader>?h",        function() Snacks.picker.command_history() end,                                desc = "Show Command History" },
      { "<leader>?m",        function() Snacks.picker.man() end,                                            desc = "Show Man Pages" },
      { "<leader>?j",        function() Snacks.picker.jumps() end,                                          desc = "Show Jump Lists" },
      { "<leader>?l",        function() Snacks.picker.loclist() end,                                        desc = "Show Location List" },
      { "<leader>?s",        function() Snacks.picker.search_history() end,                                 desc = "Show Search History" },
      { "<leader>?r",        function() Snacks.picker.registers() end,                                      desc = "Show Registers" },
      { "<leader>?M",        function() Snacks.picker.marks() end,                                          desc = "Show Marks" },
    },
  }
}
