return {
  {
    'numToStr/Comment.nvim',
    keys = {
      { 'gcc', mode = { 'n', }, function() require('Comment').toggle() end, desc = "Comment" },
      { 'gc',  mode = { 'v' },  function() require('Comment').toggle() end, desc = "Comment" },
    },
    config = true
  },
  {
    "kylechui/nvim-surround",
    keys = { 'cs', 'ds', 'ys' },
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    config = true,
  },
}
