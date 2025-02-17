return {
  {
    "nvim-lualine/lualine.nvim",
    enabled = false,
    config = function()
      require("lualine").setup({
        options = {
          theme = 'tokyonight',
          component_separators = { left = "", right = "" },
          globalstatus = true,
        },
        sections = {
          lualine_a = { 'mode' },
          lualine_b = {},
          lualine_c = { { 'filename', path = 1 }, 'diagnostics' },
          lualine_x = { 'encoding', 'filetype', 'location' },
          lualine_y = {},
          lualine_z = {}
        },
      })
      vim.opt.showmode = false -- avoid duplicate showings of mode
      vim.opt.showcmd = false  -- don't show commands'
    end
  },
  {
    'echasnovski/mini.statusline',
    enabled = true,
    version = false,
    config = function()
      require("mini.statusline").setup({
        content = {
          active = function()
            local mode, mode_hl = MiniStatusline.section_mode({ trunc_width = 200 })
            local diagnostics   = MiniStatusline.section_diagnostics({ trunc_width = 80 })
            local lsp           = MiniStatusline.section_lsp({ trunc_width = 80 })
            local filename      = MiniStatusline.section_filename({ trunc_width = 120 })
            local fileinfo      = MiniStatusline.section_fileinfo({ trunc_width = 120 })
            local location      = MiniStatusline.section_location({ trunc_width = 200 })

            return MiniStatusline.combine_groups({
              { hl = mode_hl,                  strings = { mode } },
              { hl = 'MiniStatuslineFilename', strings = { filename } },
              { hl = 'MiniStatuslineDevinfo',  strings = { diagnostics } },
              { hl = 'MiniStatuslineFilename', },
              '%<', -- Mark general truncate point
              '%=', -- End left alignment
              { hl = 'MiniStatuslineDevinfo',  strings = { lsp } },
              { hl = 'MiniStatuslineFilename', strings = { fileinfo, location } },
              -- { hl = 'MiniStatuslineFileinfo', strings = { fileinfo, location } },
            })
          end
        }
      })

      vim.opt.showmode = false -- avoid duplicate showings of mode
      vim.opt.showcmd = false  -- don't show commands'
    end
  },
}
