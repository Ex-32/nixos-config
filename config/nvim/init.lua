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
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',

  {
    "willothy/flatten.nvim",
    opts = {},
    -- Ensure that it runs first to minimize delay when opening file from terminal
    lazy = false,
    priority = 1001,
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
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
      return {
        sources = {
          null_ls.builtins.code_actions.statix,
          null_ls.builtins.diagnostics.mypy,
          null_ls.builtins.formatting.alejandra,
          null_ls.builtins.formatting.clang_format,
          null_ls.builtins.formatting.prettier,
        },
        -- on_attach = function(client, bufnr)
        --   if client.supports_method("textDocument/formatting") then
        --     vim.api.nvim_clear_autocmds({
        --       group = augroup,
        --       buffer = bufnr,
        --     })
        --     vim.api.nvim_create_autocmd("BufWritePre", {
        --       group = augroup,
        --       buffer = bufnr,
        --       callback = function()
        --         vim.lsp.buf.format({ bufnr = bufnr })
        --       end,
        --     })
        --   end
        -- end
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
    -- a soothing pastel theme for the high-spirited!
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
    opts = {
      flavor = "mocha",
      transparent_background = false,
      show_end_of_buffer = true,
    },

    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin")
    end
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
    opts = {}
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
      open_mapping = [[<c-\>]],
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
  {
    "simrat39/rust-tools.nvim",
    dependencies = {
      "neovim/nvim-lspconfig",
      "hrsh7th/cmp-nvim-lsp",
    },
    ft = "rust",
    opts = {
      tools = {
        inlay_hints = {
          other_hints_prefix = "󰆧 ",
        },
      },
    },
  },
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

  {
    -- silly goofy plugin to enable discord rich presence integration
    "andweeb/presence.nvim",
    event = "VeryLazy",
    opts = {
      neovim_image_text = "A hyperextensible Vim-based text editor",
    }
  },
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
-- since LSPs usually take a while to spin up anyway, there's no reason to not delay the initialization of the LSP interface until after the first render
-- lspconfig object
local lspconfig = require('lspconfig')

-- LSP servers to be loaded
local servers = {
  arduino_language_server = {},
  bashls = {},
  clangd = {
    on_attach = function(client, _)
      client.capabilities.signatureHelpProvider = false
    end
  },
  cmake = {
    filetypes = { "cmake", "CMakeLists.txt" },
  },
  eslint = {
    on_attach = function(_, buffer)
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = buffer,
        command = "EslintFixAll",
      })
    end,
  },
  fortls = {},
  gopls = {
    cmd = { "gopls" },
    filetypes = { "go", "gomod", "gowork", "gotmpl" },
    root_dir = lspconfig.util.root_pattern("go.work", "go.mod", ".git"),
    settings = {
      gopls = {
        completeUnimported = true,
        analyses = {
          unusedparams = true,
        },
      },
    },
  },
  jedi_language_server = {},
  kotlin_language_server = {},
  lua_ls = {
    settings = {
      Lua = {
        workspace = { checkThirdParty = false },
        telemetry = { enable = false },
        diagnostics = {
          disable = { 'missing-fields' },
        },
      },
    },
  },
  nixd = {},
  pyright = {
    filetypes = { "python" },
    root_dir = lspconfig.util.root_pattern(".venv", ".git"),
  },
  texlab = {},
  tsserver = {},
}


-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = require('cmp_nvim_lsp').default_capabilities(
  vim.lsp.protocol.make_client_capabilities()
)

-- actually register/configure the LSPs
for key, value in pairs(servers) do
  value.capabilities = value.capabilities or capabilities
  lspconfig[key].setup(value)
end

-- document existing key chains
require('which-key').register {
  ['<leader>l'] = { name = '[L]SP', _ = 'which_key_ignore' },
  ['<leader>d'] = { name = '[D]ocument', _ = 'which_key_ignore' },
  ['<leader>g'] = { name = '[G]it', _ = 'which_key_ignore' },
  ['<leader>h'] = { name = 'Git [H]unk', _ = 'which_key_ignore' },
  ['<leader>r'] = { name = '[R]ename', _ = 'which_key_ignore' },
  ['<leader>f'] = { name = '[F]ind', _ = 'which_key_ignore' },
  ['<leader>t'] = { name = '[T]oggle', _ = 'which_key_ignore' },
  ['<leader>w'] = { name = '[W]orkspace', _ = 'which_key_ignore' },
  ['<leader>s'] = { name = '[S]ymbol', _ = 'which_key_ignore' },
}
-- register which-key VISUAL mode
-- required for visual <leader>hs (hunk stage) to work
require('which-key').register({
  ['<leader>'] = { name = 'VISUAL <leader>' },
  ['<leader>h'] = { 'Git [H]unk' },
}, { mode = 'v' })

local signs = { Error = "", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.diagnostic.config({
  update_in_insert = true,
  underline = true,
})

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
    ['<CR>'] = cmp.mapping.confirm {
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

-- Telescope live_grep in git root
-- Function to find the git root directory based on the current buffer's path
local function helper_find_git_root()
  -- Use the current buffer's path as the starting point for the git search
  local current_file = vim.api.nvim_buf_get_name(0)
  local current_dir
  local cwd = vim.fn.getcwd()
  -- If the buffer is not associated with a file, return nil
  if current_file == '' then
    current_dir = cwd
  else
    -- Extract the directory from the current file's path
    current_dir = vim.fn.fnamemodify(current_file, ':h')
  end

  -- Find the Git root directory from the current file's path
  local git_root = vim.fn.systemlist('git -C ' .. vim.fn.escape(current_dir, ' ') .. ' rev-parse --show-toplevel')[1]
  if vim.v.shell_error ~= 0 then
    print 'Not a git repository. Searching on current working directory'
    return cwd
  end
  return git_root
end

local mappings = {
  i = {
    -- go to  beginning and end
    ["<C-b>"] = { "<ESC>^i", "Beginning of line" },
    ["<C-e>"] = { "<End>", "End of line" },

    -- navigate within insert mode
    ["<C-h>"] = { "<Left>", "Move left" },
    ["<C-l>"] = { "<Right>", "Move right" },
    ["<C-j>"] = { "<Down>", "Move down" },
    ["<C-k>"] = { "<Up>", "Move up" },
  },
  n = {
    ["<space>"] = { "<Nop>", nil, { silent = true } },
    ["<leader>ts"] = { "<cmd> set spell! <CR>", "[T]oggle [S]pellcheck" },
    ["<Esc>"] = { "<cmd> noh <CR>", "Clear highlights" },
    ["<C-s>"] = { "<cmd> w <CR>", "Save file" },
    ["<C-c>"] = { "<cmd> %y+ <CR>", "Copy whole file" },

    ["<leader>/"] = {
      function()
        require("Comment.api").toggle.linewise.current()
      end,
      "Toggle comment",
    },

    -- icon picker mappings
    ["<leader>si"] = { "<cmd> IconPickerNormal <CR>", "[S]ymbol [I]nsert" },
    ["<leader>sy"] = { "<cmd> IconPickerYank <CR>", "[S]ymbol [Y]ank" },

    -- Diagnostic keymaps
    ["[d"] = { vim.diagnostic.goto_prev, "Go to previous diagnostic message" },
    ["]d"] = { vim.diagnostic.goto_next, "Go to next diagnostic message" },
    ["<leader>e"] = { vim.diagnostic.open_float, "Open floating diagnostic message" },
    ["<leader>q"] = { vim.diagnostic.setloclist, "Open diagnostics list" },

    -- telescope mappings
    ["<leader>ft"] = { "<cmd> TodoTelescope <CR>", "[F]ind [T]ODO tags" },
    ["<leader>?"] = { require('telescope.builtin').oldfiles, "[?] Find recently opened files" },
    ["<leader><space>"] = { require('telescope.builtin').buffers, "[ ] Find existing buffers" },
    ["<leader>fc"] = { require("telescope.builtin").current_buffer_fuzzy_find, "[F]ind in [C]urrent buffer" },
    ["<leader>fs"] = { require('telescope.builtin').builtin, "[F]ind [S]elect Telescope" },
    ["<leader>gf"] = { require('telescope.builtin').git_files, "Find [G]it [F]iles" },
    ["<leader>ff"] = { require('telescope.builtin').find_files, "[F]ind [F]iles" },
    ["<leader>fh"] = { require('telescope.builtin').help_tags, "[F]ind [H]elp" },
    ["<leader>fw"] = { require('telescope.builtin').grep_string, "[F]ind current [W]ord" },
    ["<leader>fg"] = { require('telescope.builtin').live_grep, "[F]ind by [G]rep" },
    ["<leader>fd"] = { require('telescope.builtin').diagnostics, "[F]ind in [D]iagnostics" },
    ["<leader>fr"] = { require('telescope.builtin').resume, "[F]ind [R]esume" },
    ["<leader>f/"] = {
      function()
        require('telescope.builtin').live_grep {
          grep_open_files = true,
          prompt_title = 'Live Grep in Open Files',
        }
      end,
      "[F]ind [/] in open files",
    },
    ["<leader>fG"] = {
      function()
        local git_root = helper_find_git_root()
        if git_root then
          require('telescope.builtin').live_grep {
            search_dirs = { git_root },
          }
        end
      end,
      "[F]ind by [G]rep on Git Root",
    },

    -- LSP mappings
    ["K"] = { vim.lsp.buf.hover, "Hover Documentation" },
    ["J"] = { vim.lsp.buf.signature_help, "Signature Documentation" },

    ["<leader>lr"] = { vim.lsp.buf.rename, "[L]SP: [R]ename" },
    ["<leader>lc"] = { vim.lsp.buf.code_action, "[L]SP: [C]ode Action" },
    ["<leader>ld"] = { require('telescope.builtin').lsp_definitions, "[L]SP: Goto [D]efinition" },
    ["<leader>lf"] = { require('telescope.builtin').lsp_references, "[L]SP: Goto Re[F]erences" },
    ["<leader>li"] = { require('telescope.builtin').lsp_implementations, "[L]SP: Goto [I]mplementations" },
    ["<leader>lt"] = { require('telescope.builtin').lsp_type_definitions, "[L]SP: [T]ype Definition" },
    ["<leader>lD"] = { vim.lsp.buf.declaration, "[L]SP: Goto [D]eclaration" },
    ["<leader>lo"] = { vim.lsp.buf.format, "[L]SP: F[O]rmat" },

    -- LSP workspace mappings
    ["<leader>ws"] = { require('telescope.builtin').lsp_dynamic_workspace_symbols, "[L]SP: [W]orkspace [S]ymbols" },
    ["<leader>wa"] = { vim.lsp.buf.add_workspace_folder, "[L]SP: [W]orkspace [A]dd Folder" },
    ["<leader>wr"] = { vim.lsp.buf.remove_workspace_folder, "[L]SP: [W]orkspace [R]emove Folder" },
    ["<leader>wl"] = { function() print(vim.inspect(vim.lsp.buf.list_workspace_folders)) end, "[L]SP: [W]orkspace [L]ist Folders" },

    -- Allow moving the cursor through wrapped lines with j, k, <Up> and <Down>
    -- http://www.reddit.com/r/vim/comments/2k4cbr/problem_with_gj_and_gk/
    -- empty mode is same as using <cmd> :map
    -- also don't use g[j|k] when in operator pending mode, so it doesn't alter d, y or c behaviour
    ["j"] = { 'v:count || mode(1)[0:1] == "no" ? "j" : "gj"', "Move down", { expr = true, silent = true } },
    ["k"] = { 'v:count || mode(1)[0:1] == "no" ? "k" : "gk"', "Move up", { expr = true, silent = true } },
    ["<Up>"] = { 'v:count || mode(1)[0:1] == "no" ? "k" : "gk"', "Move up", { expr = true, silent = true } },
    ["<Down>"] = { 'v:count || mode(1)[0:1] == "no" ? "j" : "gj"', "Move down", { expr = true, silent = true } },
  },
  v = {
    ["<space>"] = { "<Nop>", nil, { silent = true } },

    ["<Up>"] = { 'v:count || mode(1)[0:1] == "no" ? "k" : "gk"', "Move up", { expr = true, silent = true } },
    ["<Down>"] = { 'v:count || mode(1)[0:1] == "no" ? "j" : "gj"', "Move down", { expr = true, silent = true } },

    ["<leader>/"] = {
      "<ESC><cmd>lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>",
      "Toggle comment",
    },
  },
  x = {
    ["j"] = { 'v:count || mode(1)[0:1] == "no" ? "j" : "gj"', "Move down", { expr = true, silent = true } },
    ["k"] = { 'v:count || mode(1)[0:1] == "no" ? "k" : "gk"', "Move up", { expr = true, silent = true } },
    -- Don't copy the replaced text after pasting in visual mode
    -- https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text#Alternative_mapping_for_paste
    ["p"] = { 'p:let @+=@0<CR>:let @"=@0<CR>', "Dont copy replaced text", { silent = true } },
  },
};

for mode, mode_values in pairs(mappings) do
  for lhs, rhs in pairs(mode_values) do
    local opts = rhs[3] or {}
    opts.desc = opts.desc or rhs[2]
    vim.keymap.set(mode, lhs, rhs[1], opts)
  end
end

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
