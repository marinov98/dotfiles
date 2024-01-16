return {
  {
    'm4xshen/autoclose.nvim', -- autopairs plugin
    event = "InsertEnter",
    opts = {}                -- this is equalent to setup({}) function
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
    main = "ibl",
    config = function()
      require('ibl').setup({ scope = { enabled = false } }) -- don't highlight the current context
    end
  },
  {
   "HiPhish/rainbow-delimiters.nvim",
    event = { 'BufReadPre', 'BufNewFile' },
  }
}
