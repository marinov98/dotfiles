return {
  {
    'm4xshen/autoclose.nvim', -- autopairs plugin
    event = "InsertEnter",
    opts = {}
  },
  {
    "HiPhish/rainbow-delimiters.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
  }
}
