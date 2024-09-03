local lspconfig = require('lspconfig')

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

-- register/configure servers from lua/local/lsp.lua
for key, value in pairs(servers) do
  value.capabilities = value.capabilities or capabilities
  lspconfig[key].setup(value)
end
