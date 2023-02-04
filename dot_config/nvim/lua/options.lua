vim.opt.showmatch = true               -- show matching 
vim.opt.ignorecase = true                   -- case insensitive 
vim.opt.mouse = 'va'                 -- middle-click paste with 
vim.opt.hlsearch = true                -- highlight search 
vim.opt.incsearch = true              -- incremental search
vim.opt.tabstop = 4               -- number of columns occupied by a tab 
vim.opt.softtabstop = 4           -- see multiple spaces as tabstops so <BS> does the right thing
vim.opt.expandtab = true               -- converts tabs to white space
vim.opt.shiftwidth = 4            -- width for autoindents
vim.opt.autoindent = true              -- indent a new line the same amount as the line just typed
vim.opt.number = true                  -- add line numbers
vim.opt.wildmode = 'longest,list'   -- get bash-like tab completions
vim.opt.cc = '80'                  -- set an 80 column border for good coding style
vim.cmd("filetype plugin indent on")   --allow auto-indenting depending on file type
vim.opt.clipboard = 'unnamedplus'   -- using system clipboard
vim.cmd("filetype plugin on")
vim.opt.ttyfast = true                 -- Speed up scrolling in Vim
vim.cmd("set termguicolors")
vim.cmd("colorscheme sonokai")