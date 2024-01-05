return {
  'numToStr/Comment.nvim',
  keys = {
    { 'gcc', mode = { 'n', }, function() require('Comment').toggle() end, desc = "Comment" },
    { 'gc',  mode = { 'v' },  function() require('Comment').toggle() end, desc = "Comment" },
  },
  config = true
}
