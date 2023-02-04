return require('packer').startup(function()
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Simple plugins can be specified as strings
    use {'nvim-lualine/lualine.nvim',
    requires = { 'kyazdani42/nvim-web-devicons', opt = true }
}

use 'github/copilot.vim'
use 'HallerPatrick/py_lsp.nvim'
use 'j-hui/fidget.nvim'
use {
  "folke/which-key.nvim",
  config = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 300
    require("which-key").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  end
}

use {'nvim-telescope/telescope-fzf-native.nvim', 
run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' }

use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    -- or                            , branch = '0.1.x',
    requires = { {'nvim-lua/plenary.nvim'} }
}

use {
    'francoiscabrol/ranger.vim',
    requires = {'rbgrouleff/bclose.vim'}
}
use 'BurntSushi/ripgrep'

use {'nvim-treesitter/nvim-treesitter',
run = function() require('nvim-treesitter.install').update(
    { with_sync = true }) 
end,
    }

    use {"ahmedkhalf/project.nvim",
    config = function()
        require("project_nvim").setup {
            -- Manual mode doesn't automatically change your root directory, so you have
            -- the option to manually do so using `:ProjectRoot` command.
            manual_mode = false,

            -- Methods of detecting the root directory. **"lsp"** uses the native neovim
            -- lsp, while **"pattern"** uses vim-rooter like glob pattern matching. Here
            -- order matters: if one is not detected, the other is used as fallback. You
            -- can also delete or rearangne the detection methods.
            detection_methods = { "lsp", "pattern" },

            -- All the patterns used to detect root dir, when **"pattern"** is in
            -- detection_methods
            patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },

            -- Table of lsp clients to ignore by name
            -- eg: { "efm", ... }
            ignore_lsp = {},

            -- Don't calculate root dir on specific directories
            -- Ex: { "~/.cargo/*", ... }
            exclude_dirs = {},

            -- Show hidden files in telescope
            show_hidden = false,

            -- When set to false, you will get a message when project.nvim changes your
            -- directory.
            silent_chdir = true,

            -- Path where project.nvim will store the project history for use in
            -- telescope
            datapath = vim.fn.stdpath("data"),
        }
    end
}
use 'neovim/nvim-lspconfig'
use 'sainnhe/sonokai'
use 'preservim/vim-pencil'
use 'tpope/vim-sensible'
use 'tpope/vim-fugitive'
use 'tpope/vim-surround'
use 'tpope/vim-commentary'
use 'ryanoasis/vim-devicons'
end)
