{ config, lib, pkgs, ...}:

let
  cfg = config.userSettings.yazi;
in {
  options = {
    userSettings.yazi = {
      enable = lib.mkEnableOption "Enable yazi TUI file manager";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;
      theme.icon = {
        dirs = [
          { name = ".config"; text = ""; }
          { name = ".git"; text = ""; }
          { name = ".github"; text = ""; }
          { name = ".npm"; text = ""; }
          { name = "Desktop"; text = ""; }
          { name = "Development"; text = ""; }
          { name = "Documents"; text = ""; }
          { name = "Downloads"; text = ""; }
          { name = "Library"; text = ""; }
          { name = "Movies"; text = ""; }
          { name = "Music"; text = ""; }
          { name = "Pictures"; text = ""; }
          { name = "Public"; text = ""; }
          { name = "Videos"; text = ""; }
          { name = "nixos"; text = ""; }
          { name = "Archive"; text = ""; }
          { name = "Media"; text = ""; }
          { name = "Podcasts"; text = ""; }
          { name = "Drive"; text = ""; }
          { name = "KP"; text = ""; }
          { name = "Books"; text = ""; }
          { name = "Games"; text = ""; }
          { name = "Game Saves"; text = ""; }
          { name = "Templates"; text = ""; }
          { name = "Notes"; text = ""; }
          { name = "Projects"; text = ""; }
          { name = "Screenshots"; text = ""; }
        ];
      };
      keymap.manager.prepend_keymap =
        [
	        { run = "cd ~/Projects"; on = [ "g" "p" ]; desc = "Go to projects"; }
	        { run = "cd ~/Screenshots"; on = [ "g" "s" ]; desc = "Go to screenshots"; }
          { run = "shell ' \"$@\"' --cursor=0 --interactive"; on = [ "@" ]; }
          { run = "hidden toggle"; on = [ "<C-h>" ]; }
          { run = "yank"; on = [ "y" "y" ]; }
          { run = "copy path"; on = [ "y" "p" ]; }
          { run = "copy dirname"; on = [ "y" "d" ]; }
          { run = "copy filename"; on = [ "y" "n" ]; }
          { run = "copy name_without_ext"; on = [ "y" "N" ]; }
          { run = "yank --cut"; on = [ "d" "d" ]; }
          { run = "remove --force"; on = [ "d" "D" ]; }
          { run = "paste"; on = [ "p" "p" ]; }
          { run = "paste --force"; on = [ "p" "P" ]; }
          { run = "cd --interactive"; on = [ "c" "d" ]; }
          { run = "sort mtime --reverse=no"; on = [ "o" "m" ]; }
          { run = "sort mtime --reverse=yes"; on = [ "o" "M" ]; }
          { run = "sort natural --reverse=no"; on = [ "o" "b" ]; }
          { run = "sort natural --reverse=yes"; on = [ "o" "B" ]; }
          { run = "sort alphabetical --reverse=no"; on = [ "o" "a" ]; }
          { run = "sort alphabetical --reverse=yes"; on = [ "o" "A" ]; }
          { run = "sort extension --reverse=no"; on = [ "o" "e" ]; }
          { run = "sort extension --reverse=yes"; on = [ "o" "E" ]; }
          { run = "sort size --reverse=no"; on = [ "o" "s" ]; }
          { run = "sort size --reverse=yes"; on = [ "o" "S" ]; }
          { run = "tab_create --current"; on = [ "t" ]; }
          { run = "close"; on = [ "x" ]; }
          { run = "tab_switch 1 --relative"; on = [ "J" ]; }
          { run = "tab_switch 1 --relative"; on = [ "<C-Tab>" ]; }
          { run = "tab_switch -1 --relative"; on = [ "K" ]; }
          { run = "tab_switch -1 --relative"; on = [ "<C-BackTab>" ]; }
          { run = "undo"; on = [ "u" ]; }
          { run = "redo"; on = [ "<C-r>" ]; }
        ];
      plugins = {
        git = pkgs.yaziPlugins.git;
        sudo = pkgs.yaziPlugins.sudo;
        piper = pkgs.yaziPlugins.piper;
        yatline = pkgs.yaziPlugins.yatline;
      };
      initLua = ''
        require("git"):setup()
        require("yatline"):setup({
	--theme = my_theme,
	section_separator = { open = "", close = "" },
	part_separator = { open = "", close = "" },
	inverse_separator = { open = "", close = "" },

	style_a = {
		fg = "black",
		bg_mode = {
			normal = "white",
			select = "brightyellow",
			un_set = "brightred"
		}
	},
	style_b = { bg = "brightblack", fg = "brightwhite" },
	style_c = { bg = "black", fg = "brightwhite" },

	permissions_t_fg = "green",
	permissions_r_fg = "yellow",
	permissions_w_fg = "red",
	permissions_x_fg = "cyan",
	permissions_s_fg = "white",

	tab_width = 20,
	tab_use_inverse = false,

	selected = { icon = "󰻭", fg = "yellow" },
	copied = { icon = "", fg = "green" },
	cut = { icon = "", fg = "red" },

	total = { icon = "󰮍", fg = "yellow" },
	succ = { icon = "", fg = "green" },
	fail = { icon = "", fg = "red" },
	found = { icon = "󰮕", fg = "blue" },
	processed = { icon = "󰐍", fg = "green" },

	show_background = true,

	display_header_line = true,
	display_status_line = true,

	component_positions = { "header", "tab", "status" },

	header_line = {
		left = {
			section_a = {
                			{type = "line", custom = false, name = "tabs", params = {"left"}},
			},
			section_b = {
			},
			section_c = {
			}
		},
		right = {
			section_a = {
			},
			section_b = {
			},
			section_c = {
			}
		}
	},

	status_line = {
		left = {
			section_a = {
                			{type = "string", custom = false, name = "tab_mode"},
			},
			section_b = {
                			{type = "string", custom = false, name = "hovered_size"},
			},
			section_c = {
                			{type = "string", custom = false, name = "hovered_path"},
                			{type = "coloreds", custom = false, name = "count"},
			}
		},
		right = {
			section_a = {
                			{type = "string", custom = false, name = "cursor_position"},
			},
			section_b = {
                			{type = "string", custom = false, name = "cursor_percentage"},
			},
			section_c = {
                			{type = "string", custom = false, name = "hovered_file_extension", params = {true}},
                			{type = "coloreds", custom = false, name = "permissions"},
			}
		}
	},
        })
      '';
      settings = {
        "plugin.prepend_fetchers" = [
          {
            id = "git";
            name = "*";
            run = "git";
          }
          {
            id = "git";
            name = "*/";
            run = "git";
          }
        ];
      };
    };
  };
}
