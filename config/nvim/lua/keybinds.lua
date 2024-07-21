
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

local keybinds = {
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
    ["<leader>tl"] = { require("lsp_lines").toggle, "[T]oggle [L]SP Lines" },
    ["<leader>ti"] = {
      function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
      end,
      "[T]oggle [I]nlay Hints"
    },
    ["<Esc>"] = { "<cmd> noh <CR>", "Clear highlights" },
    ["<C-s>"] = { "<cmd> w <CR>", "Save file" },
    ["<C-c>"] = { "<cmd> %y+ <CR>", "Copy whole file" },

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
        local git_root = require("utils").find_git_root()
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
}

for mode, mode_values in pairs(keybinds) do
  for lhs, rhs in pairs(mode_values) do
    local opts = rhs[3] or {}
    opts.desc = opts.desc or rhs[2]
    vim.keymap.set(mode, lhs, rhs[1], opts)
  end
end
