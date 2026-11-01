{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: let
  # API keys Zed picks up from its environment at startup: one sops secret per
  # key, named "zed/<KEY>" as declared below. Each of these has to exist in
  # secrets.yaml, since sops-nix validates the file at activation time.
  zedApiKeys = [
    "OPENCODE_API_KEY"
    "AGENTFANCLUB_API_KEY"
    "NCSA_LUMEN_API_KEY"
  ];

  # Exports whatever decrypted successfully, so the editor still starts if a
  # secret is missing. Sourced by the wrapper script built below.
  zedKeyLoader = pkgs.writeShellScript "zed-sops-api-keys" (
    lib.concatMapStrings (key: let
      file = config.sops.secrets."zed/${key}".path;
    in ''
      if [ -r ${file} ]; then
        export ${key}="$(cat ${file})"
      fi
    '') zedApiKeys
  );
in {
  sops.secrets = lib.genAttrs (map (key: "zed/${key}") zedApiKeys) (_: {});

  programs.zed-editor = {
    enable = true;
    # The editor is launched by a generated script that loads the sops-managed
    # keys first, so no API key ever has to live in settings.json (and from
    # there into the world-readable nix store).
    package = pkgs.symlinkJoin {
      name = "zed-editor-with-sops-keys";
      paths = [pkgs.zed-editor];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        for exe in "$out"/bin/*; do
          if [ -e "$exe" ]; then
            wrapProgram "$exe" --run "source ${zedKeyLoader}"
          fi
        done
      '';
      meta = pkgs.zed-editor.meta;
      passthru = pkgs.zed-editor.passthru;
    };
    # `basher` provides bash-language-server, `tombi` the TOML language server;
    # the `toml` extension only ships a grammar.
    extensions = ["nix" "toml" "tombi" "basher" "elixir" "make"];
    ## everything inside of these brackets are Zed options.
    userSettings = let
      binary = pkg: name: "${pkg}/bin/${name}";
      # Custom OpenAI-compatible providers need their catalog spelled out; only
      # the first-class `opencode` provider discovers models on its own. These
      # ids and windows are lifted from omp's cache in ~/.omp/agent
      # (models.yml + models.db), so they track what omp can actually reach.
      model = name: display_name: max_tokens: max_output_tokens: {
        inherit name display_name max_tokens max_output_tokens;
      };
    in {
      agent = {
        enabled = true;
        # "<provider>/<model>"; OpenCode ids are "<subscription>/<model>"
        default_model = {
          provider = "opencode";
          model = "go/glm-5.3-flash";
        };
      };

      language_models = {
        # Zed discovers the Zen and Go models, their context windows and their
        # per-model transport from OpenCode itself, so nothing is enumerated
        # here.
        opencode = {
          show_zen_models = false;
          show_go_models = true;
        };

        # Plain OpenAI chat-completions routers, declared as custom providers.
        # Keys are read at startup from the upper-snake provider id plus
        # _API_KEY (AGENTFANCLUB_API_KEY, NCSA_LUMEN_API_KEY), or from the
        # keychain after entering them in Agent Settings -> LLM Providers.
        openai_compatible = {
          agentfanclub = {
            api_url = "https://api.router.agentfan.club/inference/v1";
            available_models = [
              (model "deepseek-v4.1-flash" "DeepSeek V4.1 Flash" 1048570 384000)
              (model "deepseek-v4-flash" "DeepSeek V4 Flash" 511994 128000)
              (model "glm-5.3" "GLM-5.3" 524282 131072)
              (model "qwen3.8-flash" "Qwen3.8 Flash" 262138 131072)
            ];
          };
          ncsa-lumen = {
            api_url = "https://lumen.ncsa.illinois.edu/v1";
            available_models = [
              (model "beam-1.0" "beam-1.0" 524282 131072)
              (model "deepseek" "deepseek" 1048570 384000)
              (model "deepseek-v4-flash" "deepseek-v4-flash" 511994 128000)
              (model "deepseek-v4.1" "deepseek-v4.1" 1048570 384000)
              (model "deepseek-v4.1-flash" "DeepSeek V4.1 Flash" 1048570 384000)
              (model "gemma-4-31b-it" "Gemma 4 31B IT" 46790 32768)
              (model "glm" "glm" 524282 131072)
              (model "glm-5.3" "GLM-5.3" 524282 131072)
              (model "granite-speech-4.1-2b-plus" "granite-speech-4.1-2b-plus" 4096 16384)
              (model "muse-glimmer-30b" "Muse Glimmer 30B" 131066 131072)
              (model "nemotron-3-super-120b-a12b" "nemotron-3-super-120b-a12b" 262144 262144)
              (model "ornith-1.0-35b" "ornith-1.0-35b" 262138 256000)
              (model "qwen3-coder-next" "Qwen3 Coder Next" 131066 256000)
              (model "qwen3.6-35b-a3b" "Qwen3.6-35B-A3B" 262138 131056)
              (model "qwen3.8-27b" "Qwen3.8-27B" 250542 262144)
              (model "qwen3.8-flash" "Qwen3.8 Flash" 262138 131072)
            ];
          };
        };
      };

      node = {
        path = lib.getExe pkgs.nodejs;
        npm_path = lib.getExe' pkgs.nodejs "npm";
      };

      hour_format = "hour24";
      auto_update = false;
      terminal = {
        alternate_scroll = "off";
        blinking = "off";
        copy_on_select = false;
        dock = "bottom";
        detect_venv = {
          on = {
            directories = [".env" "env" ".venv" "venv"];
            activate_script = "default";
          };
        };
        env = {
          TERM = "alacritty";
        };
        font_family = "FiraCode Nerd Font";
        font_features = null;
        font_size = null;
        line_height = "comfortable";
        option_as_meta = false;
        button = false;
        shell = "system";
        # {
        #   program = "zsh";
        # };
        toolbar = {
          title = true;
        };
        working_directory = "current_project_directory";
      };

      # NOTE: setting `binary.path` skips Zed's own server installation and PATH
      # lookup, and drops the arguments the built-in adapter or extension would
      # have passed, so every overridden server must spell its arguments out.
      lsp = {
        rust-analyzer = {
          binary = {
            path = lib.getExe pkgs.rust-analyzer;
          };
        };
        nixd = {
          binary = {
            path = lib.getExe pkgs.nixd;
          };
        };
        tombi = {
          binary = {
            path = lib.getExe pkgs.tombi;
            arguments = ["lsp"];
          };
        };
        basedpyright = {
          binary = {
            path = binary pkgs.basedpyright "basedpyright-langserver";
            arguments = ["--stdio"];
          };
        };
        ruff = {
          binary = {
            path = lib.getExe pkgs.ruff;
            arguments = ["server"];
          };
        };
        "bash-language-server" = {
          binary = {
            path = lib.getExe pkgs.bash-language-server;
            arguments = ["start"];
          };
        };
        elixir-ls = {
          binary = {
            path = lib.getExe pkgs.elixir-ls;
          };
          settings = {
            dialyzerEnabled = true;
          };
        };
        clangd = {
          binary = {
            path = binary pkgs.clang-tools "clangd";
          };
        };
      };

      languages = {
        "Nix" = {
          # the nix extension registers both nixd and nil; nil needs a manual install
          language_servers = ["nixd" "!nil"];
        };
        "Elixir" = {
          language_servers = ["!lexical" "elixir-ls" "!next-ls"];
          format_on_save = {
            external = {
              command = "mix";
              arguments = ["format" "--stdin-filename" "{buffer_path}" "-"];
            };
          };
        };
        "HEEX" = {
          language_servers = ["!lexical" "elixir-ls" "!next-ls"];
          format_on_save = {
            external = {
              command = "mix";
              arguments = ["format" "--stdin-filename" "{buffer_path}" "-"];
            };
          };
        };
      };

      vim_mode = true;
      ## tell zed to use direnv and direnv can use a flake.nix enviroment.
      load_direnv = "shell_hook";
      base_keymap = "VSCode";
      theme = {
        mode = "dark";
        light = "Atelier Dune Light";
        dark = "Atelier Dune Dark";
      };
      show_whitespaces = "all";
      ui_font_size = 18;
      buffer_font_size = 18;
    };
  };
}
