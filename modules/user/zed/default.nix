{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.userSettings.zed;
in
{
  options = {
    userSettings.zed = {
      enable = lib.mkEnableOption "Enable zed editor";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      gitu
    ];
    stylix.targets.zed.enable = true;
    programs.zed-editor.enable = true;
    programs.zed-editor.userSettings = {
      cursor_blink = false;
      session = {
        restore_unsaved_buffers = false;
      };
      restore_on_startup = "none";
      toolbar = {
        code_actions = true;
        agent_review = false;
        breadcrumbs = true;
      };
      minimap = {
        max_width_columns = 80;
        thumb = "always";
        show = "always";
      };
      search = {
        regex = false;
      };
      use_smartcase_search = true;
      close_on_file_delete = true;
      file_finder = {
        modal_max_width = "large";
      };
      gutter = {
        min_line_number_digits = 4;
      };
      show_whitespaces = "trailing";
      tab_bar = {
        show = false;
        show_nav_history_buttons = false;
      };
      use_system_prompts = false;
      use_system_path_prompts = false;
      auto_update = false;
      telemetry = {
        diagnostics = false;
        metrics = false;
      };
      tab_size = 2;
      disable_ai = true;
      agent = {
        button = false;
        model_parameters = [ ];
      };
      git_panel = {
        button = false;
      };
      project_panel = {
        starts_open = false;
        auto_fold_dirs = false;
      };
      active_pane_modifiers = {
        border_size = 0.0;
        inactive_opacity = 1.0;
      };
      bottom_dock_layout = "contained";
      title_bar = {
        show_menus = false;
        show_branch_icon = true;
      };
      preview_tabs = {
        enable_preview_from_file_finder = false;
      };
      tabs = {
        file_icons = true;
        git_status = true;
        show_close_button = "always";
        activate_on_close = "left_neighbour";
      };
      prettier = {
        allowed = false;
      };
      vim_mode = true;
      base_keymap = "Emacs";
      icon_theme = "Zed (Default)";
      buffer_font_family = "FiraCode Nerd Font";
      buffer_font_size = 24.0;
      ui_font_family = "Fira Sans";
      ui_font_size = 16.0;
    };
    programs.zed-editor.userKeymaps = [
      {
        context = "";
        bindings = {
          "alt-x" = "command_palette::Toggle";
          "alt-z" = "terminal_panel::Toggle";
          "alt-d" = "debug_panel::ToggleFocus";
        };
      }
      {
        context = "VimControl || (!Editor && !menu && !Terminal)";
        bindings = {
          "ctrl-x ctrl-s" = "workspace::Save";
          "ctrl-x ctrl-shift-s" = "workspace::SaveAll";
          "space w v" = "pane::SplitVertical";
          "space w V" = "pane::SplitVertical";
          "space w h" = "pane::SplitHorizontal";
          "space w H" = "pane::SplitHorizontal";
          "space p f" = "file_finder::Toggle";
          "space p p" = "projects::OpenRecent";
          "space g g" = [
            "task::Spawn"
            {
              "task_name" = "gitu";
            }
          ];
          "space g G" = [
            "task::Spawn"
            {
              "task_name" = "magit";
            }
          ];
          "space o d" = [
            "task::Spawn"
            {
              "task_name" = "yazi";
            }
          ];
          "ctrl-h" = "workspace::ActivatePaneLeft";
          "ctrl-l" = "workspace::ActivatePaneRight";
          "ctrl-j" = "workspace::ActivatePaneDown";
          "ctrl-k" = "workspace::ActivatePaneUp";
          "ctrl-w" = "pane::CloseActiveItem";
          "space w d" = "pane::CloseAllItems";
          "space b d" = "pane::CloseActiveItem";
          "space b p" = "panel::PreviousPanelTab";
          "space b n" = "panel::NextPanelTab";
          "space b i" = "tab_switcher::ToggleAll";
          "ctrl-i" = "tab_switcher::ToggleAll";
          "alt-z" = "terminal_panel::Toggle";
          "alt-d" = "debug_panel::ToggleFocus";
        };
      }
      {
        context = "OutlinePanel";
        bindings = {
          "ctrl-h" = "workspace::ActivatePaneLeft";
          "ctrl-l" = "workspace::ActivatePaneRight";
          "ctrl-j" = "workspace::ActivatePaneDown";
          "ctrl-k" = "workspace::ActivatePaneUp";
        };
      }
      {
        context = "OutlinePanel && not_editing";
        bindings = {
          "q" = "workspace::ToggleLeftDock";
        };
      }
      {
        context = "ProjectPanel";
        bindings = {
          "ctrl-h" = "workspace::ActivatePaneLeft";
          "ctrl-l" = "workspace::ActivatePaneRight";
          "ctrl-j" = "workspace::ActivatePaneDown";
          "ctrl-k" = "workspace::ActivatePaneUp";
        };
      }
      {
        context = "ProjectPanel && not_editing";
        bindings = {
          "tab" = "project_panel::OpenPermanent";
          "q" = "workspace::ToggleLeftDock";
          "r" = "project_panel::Rename";
          "D" = "project_panel::Delete";
        };
      }
      {
        context = "Terminal";
        bindings = {
          "ctrl-space w d" = "pane::CloseAllItems";
          "ctrl-space b d" = "pane::CloseActiveItem";
          "ctrl-space b p" = "panel::PreviousPanelTab";
          "ctrl-space b n" = "panel::NextPanelTab";
          "ctrl-space b i" = "tab_switcher::ToggleAll";
          "ctrl-w" = "pane::CloseActiveItem";
          "alt-z" = "terminal_panel::Toggle";
          "alt-d" = "debug_panel::ToggleFocus";
          "ctrl-i" = "tab_switcher::ToggleAll";
          "alt-p" = [
            "tab_switcher::Toggle"
            { "select_last" = true; }
          ];
          "ctrl-p" = null;
          "ctrl-n" = null;
        };
      }
      {
        "context" = "Editor && vim_mode == insert";
        "bindings" = {
          "ctrl-w" = "pane::CloseActiveItem";
          "alt-z" = "terminal_panel::Toggle";
          "alt-d" = "debug_panel::ToggleFocus";
        };
      }
    ];
    programs.zed-editor.userTasks = [
      {
        label = "gitu";
        command = "gitu || {git init && gitu}";
        reveal = "always";
        reveal_target = "center";
        allow_concurrent_runs = false;
        use_new_terminal = false;
        hide = "on_success";
      }
      {
        label = "magit";
        command = "emacsclient -c --eval '(magit-status)' &> /dev/null & disown; exit;";
        reveal = "always";
        reveal_target = "center";
        allow_concurrent_runs = false;
        use_new_terminal = false;
        hide = "on_success";
      }
      {
        label = "yazi";
        command = "yazi";
        reveal = "always";
        reveal_target = "center";
        allow_concurrent_runs = false;
        use_new_terminal = false;
        hide = "on_success";
      }
    ];
    programs.zed-editor.extensions = [
      "nix"
      "gdscript"
      "git_firefly"
      "toml"
      "xml"
      "svelte"
      "vue"
      "scss"
      "make"
      "dockerfile"
      "docker-compose"
      "hyprlang"
      "java"
      "lua"
      "r"
      "kotlin"
      "haskell"
      "perl"
      "fortran"
      "ruby"
      "org"
    ];
  };
}
