-- Set <space> as the leader key
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Install `lazy.nvim` plugin manager ]]
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

-- [[ Configure plugins ]]
require('lazy').setup({
  -- Git related plugins
  "tpope/vim-fugitive",
  "tpope/vim-rhubarb",

  "rafcamlet/nvim-luapad",

  {
    "willothy/flatten.nvim",
    -- Ensure that it runs first to minimize delay when opening file from terminal
    lazy = false,
    priority = 1001,
  },

  {
    "scottmckendry/cyberdream.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      transparent = true,
      italic_comments = true,
      borderless_telescope = true,
    },
    config = function(_, opts)
      require("cyberdream").setup(opts)
      vim.cmd("colorscheme cyberdream")
    end,
  },

  {
    "nmac427/guess-indent.nvim",
    lazy = false,
    opts = {
      auto_cmd = true,
      override_editorconfig = false,
    },
  },

  {
    -- LSP Configuration & Plugins
    'neovim/nvim-lspconfig',
    dependencies = {
      -- Useful status updates for LSP
      {
        'j-hui/fidget.nvim',
        opts = {
          notification = {
            window = {
              winblend = 0, -- transparent background for notification window
            },
          },
        },
      },

      -- Additional lua configuration, makes nvim stuff amazing!
      {
        "folke/neodev.nvim",
        opts = {
          override = function(root_dir, library)
            if root_dir:find("/etc/nixos", 1, true) == 1 then
              library.enabled = true
              library.plugins = true
            end
          end
        },
      },
    },
  },

  {
    -- Autocompletion
    'hrsh7th/nvim-cmp',
    dependencies = {
      -- Snippet Engine & its associated nvim-cmp source
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',

      -- Adds LSP completion capabilities
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',

      -- Adds a number of user-friendly snippets
      'rafamadriz/friendly-snippets',
    },
    opts = {
      sources = {
        { name = "crates" },
      },
    },
  },

  {
    -- non-LSP diagnostics and formatting tools
    -- (community maintained fork of null-ls)
    "nvimtools/none-ls.nvim",
    opts = function()
      local null_ls = require('null-ls')
      return {
        sources = {
          null_ls.builtins.code_actions.statix,
          null_ls.builtins.diagnostics.mypy,
          null_ls.builtins.formatting.alejandra,
          null_ls.builtins.formatting.clang_format,
          null_ls.builtins.formatting.prettier,
        },
      }
    end
  },

  {
    -- Useful plugin to show you pending keybinds.
    'folke/which-key.nvim',
    opts = {}
  },

  {
    -- Adds git related signs to the gutter, as well as utilities for managing changes
    'lewis6991/gitsigns.nvim',
    opts = {},
  },

  {
    "chentoast/marks.nvim",
    opts = {},
  },

  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
  },

  {
    "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
    config = function(_, _)
      require("lsp_lines").setup()
      vim.diagnostic.config({ virtual_text = false })
    end,
  },

  {
    -- highlight color codes in that color
    "brenoprata10/nvim-highlight-colors",
    opts = {
      enable_named_colors = false,
    },
  },

  {
    -- Set lualine as statusline
    'nvim-lualine/lualine.nvim',
    opts = {
      options = {
        globalstatus = true,
        icons_enabled = true,
      },
    },
  },

  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons", -- optional dependency
    },
    after = "catppuccin/nvim",
    opts = {},
  },

  {
    "willothy/nvim-cokeline",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "kyazdani42/nvim-web-devicons",
    },
    opts = {
      show_if_buffers_are_at_least = 2,
    },
  },

  {
    -- Add indentation guides even on blank lines
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    opts = {},
  },

  {
    -- utility plugin for (un)commenting lines/blocks
    'numToStr/Comment.nvim',
    opts = {},
  },

  {
    -- utility to highlight marker comments
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    'nvim-telescope/telescope.nvim',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      -- Fuzzy Finder Algorithm which requires local dependencies to be built.
      -- Only load if `make` is available. Make sure you have the system
      -- requirements installed.
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
  },

  {
    "ziontee113/icon-picker.nvim",
    opts = {
      disable_legacy_command = true,
    },
    dependencies = {
      {
        "stevearc/dressing.nvim",
        opts = {},
      }
    },
  },

  {
    "akinsho/toggleterm.nvim",
    opts = {
      open_mapping = "<c-\\>",
      direction = "float",
      shell = vim.env.SHELL,
    },
  },

  {
    -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
      "nushell/tree-sitter-nu",
    },
    build = ':TSUpdate',
  },

  {
    -- show context at top of buffer
    "nvim-treesitter/nvim-treesitter-context",
    event = "VeryLazy",
    opts = {},
  },

  {
    -- silly goofy plugin to enable discord rich presence integration
    "andweeb/presence.nvim",
    event = "VeryLazy",
    opts = {
      neovim_image_text = "A hyperextensible Vim-based text editor",
    }
  },

  -- language specific plugins
  -- (these plugins provide extra features that just a language server can't)

  ---- haskell
  {
    "MrcJkb/haskell-tools.nvim",
    version = "^3",
    ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
    setup = function(_, _)
      vim.g.haskell_tools = {
        hls = {
          settings = {
            haskell = {
              formattingProvider = "fourmolu",
            },
          },
        },
      }
    end
  },

  ---- rust
  "mrcjkb/rustaceanvim",
  {
    "saecki/crates.nvim",
    ft = { "rust", "toml" },
    config = function(_, opts)
      local crates = require("crates")
      crates.setup(opts)
      crates.show()
    end
  },

  ---- kitty config script
  {
    "fladson/vim-kitty",
    ft = { "kitty", "kitty.conf" },
  },

  ---- markdown
  {
    "OXY2DEV/markview.nvim",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },
  },

  ---- xonsh
  "meatballs/vim-xonsh",
}, {})

-- [[ Setting options ]]

vim.o.hlsearch = true           -- highlight on search
vim.o.number = true             -- enable line number
vim.o.relativenumber = true     -- relative line numbers
vim.o.mouse = "a"               -- enable mouse mode
vim.o.clipboard = "unnamedplus" -- sync clipboard between OS and neovim
vim.o.breakindent = true        -- enable break indent
vim.o.undofile = true           -- save undo history
vim.wo.signcolumn = "yes"       -- enable gutter with stuff like git flags
vim.o.spelllang = "en_us,cjk"   -- set spellcheck language
vim.o.spellsuggest = "best,5"   -- set spellcheck suggestion options
vim.o.spell = true              -- enable spellcheck
vim.o.colorcolumn = "80,100"    -- add ruler at 80 columns
vim.o.termguicolors = true      -- enable 255 terminal colors
vim.o.wrap = true               -- soft wrap lines that are too long to display
vim.o.linebreak = true          -- try to break long lines at word boundaries
vim.o.shiftwidth = 4            -- tab == 4 spaces
vim.o.conceallevel = 1

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true
-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300
-- Set completeopt to have a better completion experience
vim.o.completeopt = "menuone,noselect"

-- disable spellcheck and line numbers on terminal windows
vim.api.nvim_create_autocmd("TermOpen", {
  callback = function()
    vim.opt_local.spell = false
    vim.opt_local.relativenumber = false
    vim.opt_local.number = false
  end
})

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
      },
    },
  },
}

-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- [[ Configure Treesitter ]]
-- See `:help nvim-treesitter`
-- Defer Treesitter setup after first render to improve startup time of 'nvim {filename}'
vim.defer_fn(function()
  require('nvim-treesitter.configs').setup {
    -- Add languages to be installed here that you want installed for treesitter
    ensure_installed = {
      'arduino', 'bash', 'c', 'c_sharp', 'cmake', 'cpp', 'css', 'csv', 'cuda',
      'diff', 'dockerfile', 'fish', 'fortran', 'git_config', 'git_rebase',
      'gitattributes', 'gitcommit', 'gitignore', 'go', 'gomod', 'haskell',
      'html', 'java', 'javascript', 'json', 'jsonc', 'latex', 'linkerscript',
      'lua', 'make', 'markdown', 'markdown_inline', 'nasm', 'nix', 'nu', 'org',
      'passwd', 'python', 'rust', 'scss', 'sql', 'ssh_config', 'toml', 'tsx',
      'typescript', 'udev', 'verilog', 'vim', 'vimdoc', 'xml', 'yuck', 'zig',
    },

    -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
    auto_install = false,

    highlight = { enable = true },
    indent = { enable = true },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = '<c-space>',
        node_incremental = '<c-space>',
        scope_incremental = '<c-s>',
        node_decremental = '<M-space>',
      },
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['aa'] = '@parameter.outer',
          ['ia'] = '@parameter.inner',
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
      swap = {
        enable = true,
        swap_next = {
          ['<leader>a'] = '@parameter.inner',
        },
        swap_previous = {
          ['<leader>A'] = '@parameter.inner',
        },
      },
    },
  }
end, 0)

-- [[ Configure LSP ]]
require('lsp')

local signs = { Error = "", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.diagnostic.config({
  update_in_insert = true,
  underline = true,
})
vim.lsp.inlay_hint.enable(true)

-- [[ Configure nvim-cmp ]]
-- See `:help cmp`
local cmp = require 'cmp'
local luasnip = require 'luasnip'
require('luasnip.loaders.from_vscode').lazy_load()
luasnip.config.setup {}

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  completion = {
    completeopt = 'menu,menuone,noinsert',
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete {},
    ['<S-CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.locally_jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
    { name = 'path' },
  },
}

require("keybinds")

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
