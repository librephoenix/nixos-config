{ config, lib, pkgs, ... }:

let
  cfg = config.userSettings.godot;
in {
  options = {
    userSettings.godot = {
      enable = lib.mkEnableOption "Enable godot";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      godot_4
    ];

    # ~/.config/godot must be owned by another user in order for this to work
    # does not need to be recursively owned, however

    # TODO fix other colors
    home.file.".config/godot/editor_settings-4.3.tres".text = ''
      [gd_resource type="EditorSettings" format=3]
      
      [resource]
      interface/editor/separate_distraction_mode = true
      interface/theme/preset = "Custom"
      interface/theme/spacing_preset = "Custom"
      interface/theme/base_color = Color(''+config.lib.stylix.colors.base00-dec-r+'',''+config.lib.stylix.colors.base00-dec-g+'', ''+config.lib.stylix.colors.base00-dec-b+'', 0.8)

      interface/theme/accent_color = Color(''+config.lib.stylix.colors.base08-dec-r+'',''+config.lib.stylix.colors.base08-dec-g+'', ''+config.lib.stylix.colors.base08-dec-b+'', 1)

      interface/theme/contrast = 0.3
      interface/theme/icon_saturation = 1.55
      interface/theme/relationship_line_opacity = 0.35
      interface/theme/border_size = 1
      interface/theme/corner_radius = 6
      interface/theme/additional_spacing = 1
      interface/touchscreen/enable_pan_and_scale_gestures = true
      interface/touchscreen/scale_gizmo_handles = 2.0
      interface/multi_window/enable = false
      interface/multi_window/restore_windows_on_load = false
      filesystem/external_programs/raster_image_editor = "krita"
      filesystem/external_programs/vector_image_editor = "inkscape"
      filesystem/external_programs/3d_model_editor = "blender"
      filesystem/external_programs/terminal_emulator = "''+config.userSettings.terminal+''"

      filesystem/directories/default_project_path = "/home/''+config.home.username+''/Projects"

      text_editor/theme/highlighting/symbol_color = Color(0.67, 0.79, 1, 1)
      text_editor/theme/highlighting/keyword_color = Color(1, 0.44, 0.52, 1)
      text_editor/theme/highlighting/control_flow_keyword_color = Color(1, 0.55, 0.8, 1)
      text_editor/theme/highlighting/base_type_color = Color(0.26, 1, 0.76, 1)
      text_editor/theme/highlighting/engine_type_color = Color(0.56, 1, 0.86, 1)
      text_editor/theme/highlighting/user_type_color = Color(0.78, 1, 0.93, 1)
      text_editor/theme/highlighting/comment_color = Color(0.764706, 0.769608, 0.77451, 0.5)
      text_editor/theme/highlighting/doc_comment_color = Color(0.6, 0.7, 0.8, 0.8)
      text_editor/theme/highlighting/string_color = Color(1, 0.93, 0.63, 1)
      text_editor/theme/highlighting/background_color = Color(0.0323529, 0.0431373, 0.0539216, 1)
      text_editor/theme/highlighting/completion_background_color = Color(0.0588235, 0.0784314, 0.0980392, 1)
      text_editor/theme/highlighting/completion_selected_color = Color(1, 1, 1, 0.07)
      text_editor/theme/highlighting/completion_existing_color = Color(1, 1, 1, 0.14)
      text_editor/theme/highlighting/completion_font_color = Color(0.764706, 0.769608, 0.77451, 1)
      text_editor/theme/highlighting/text_color = Color(0.764706, 0.769608, 0.77451, 1)
      text_editor/theme/highlighting/line_number_color = Color(0.764706, 0.769608, 0.77451, 0.5)
      text_editor/theme/highlighting/safe_line_number_color = Color(0.764706, 0.923529, 0.77451, 0.75)
      text_editor/theme/highlighting/caret_color = Color(1, 1, 1, 1)
      text_editor/theme/highlighting/selection_color = Color(0.941176, 0.443137, 0.470588, 0.4)
      text_editor/theme/highlighting/brace_mismatch_color = Color(1, 0.47, 0.42, 1)
      text_editor/theme/highlighting/current_line_color = Color(1, 1, 1, 0.07)
      text_editor/theme/highlighting/line_length_guideline_color = Color(0.0588235, 0.0784314, 0.0980392, 1)
      text_editor/theme/highlighting/word_highlighted_color = Color(1, 1, 1, 0.07)
      text_editor/theme/highlighting/number_color = Color(0.63, 1, 0.88, 1)
      text_editor/theme/highlighting/function_color = Color(0.34, 0.7, 1, 1)
      text_editor/theme/highlighting/member_variable_color = Color(0.736, 0.88, 1, 1)
      text_editor/theme/highlighting/mark_color = Color(1, 0.47, 0.42, 0.3)
      text_editor/theme/highlighting/breakpoint_color = Color(1, 0.47, 0.42, 1)
      text_editor/theme/highlighting/code_folding_color = Color(1, 1, 1, 0.27)
      text_editor/theme/highlighting/search_result_color = Color(1, 1, 1, 0.07)
      text_editor/appearance/whitespace/draw_tabs = false
      text_editor/behavior/indent/type = 1
      text_editor/behavior/indent/size = 2
      text_editor/behavior/files/trim_trailing_whitespace_on_save = true
      editors/panning/2d_editor_panning_scheme = 1
      editors/panning/sub_editors_panning_scheme = 1
      editors/panning/simple_panning = true
      project_manager/directory_naming_convention = 3
      asset_library/available_urls = {
      "godotengine.org (Official)": "https://godotengine.org/asset-library/api"
      }
      asset_library/use_threads = true
      export/android/java_sdk_path = ""
      export/android/android_sdk_path = ""
      export/android/debug_keystore = "/home/emmet/.local/share/godot/keystores/debug.keystore"
      export/android/debug_keystore_user = "androiddebugkey"
      export/android/debug_keystore_pass = "android"
      export/android/force_system_user = false
      export/android/shutdown_adb_on_exit = true
      export/android/one_click_deploy_clear_previous_install = false
      export/android/use_wifi_for_remote_debug = false
      export/android/wifi_remote_debug_host = "localhost"
      export/macos/rcodesign = ""
      export/web/http_host = "localhost"
      export/web/http_port = 8060
      export/web/use_tls = false
      export/web/tls_key = ""
      export/web/tls_certificate = ""
      export/windows/rcedit = ""
      export/windows/osslsigncode = ""
      export/windows/wine = "/home/emmet/.nix-profile/bin/wine64"
      _default_feature_profile = ""
      interface/editors/show_scene_tree_root_selection = true
      interface/editors/derive_script_globals_by_name = true
      docks/scene_tree/ask_before_deleting_related_animation_tracks = true
      _use_favorites_root_selection = false
      filesystem/file_server/port = 6010
      filesystem/file_server/password = ""
      editors/3d/manipulator_gizmo_size = 80
      editors/3d/manipulator_gizmo_opacity = 0.9
      editors/3d/navigation/show_viewport_rotation_gizmo = true
      editors/3d/navigation/show_viewport_navigation_gizmo = false
      text_editor/behavior/files/auto_reload_and_parse_scripts_on_save = true
      text_editor/behavior/files/open_dominant_script_on_scene_change = false
      text_editor/external/use_external_editor = false
      text_editor/external/exec_path = ""
      text_editor/script_list/script_temperature_enabled = true
      text_editor/script_list/script_temperature_history_size = 15
      text_editor/script_list/group_help_pages = true
      text_editor/script_list/sort_scripts_by = 0
      text_editor/script_list/list_script_names_as = 0
      text_editor/external/exec_flags = "{file}"
      version_control/username = ""
      version_control/ssh_public_key_path = ""
      version_control/ssh_private_key_path = ""
      editors/bone_mapper/handle_colors/unset = Color(0.3, 0.3, 0.3, 1)
      editors/bone_mapper/handle_colors/set = Color(0.1, 0.6, 0.25, 1)
      editors/bone_mapper/handle_colors/missing = Color(0.8, 0.2, 0.8, 1)
      editors/bone_mapper/handle_colors/error = Color(0.8, 0.2, 0.2, 1)
      network/debug_adapter/remote_port = 6006
      network/debug_adapter/request_timeout = 1000
      network/debug_adapter/sync_breakpoints = false
      editors/3d_gizmos/gizmo_settings/path3d_tilt_disk_size = 0.8
      editors/3d_gizmos/gizmo_colors/path_tilt = Color(1, 1, 0.4, 0.9)
      editors/3d_gizmos/gizmo_colors/skeleton = Color(1, 0.8, 0.4, 1)
      editors/3d_gizmos/gizmo_colors/selected_bone = Color(0.8, 0.3, 0, 1)
      editors/3d_gizmos/gizmo_settings/bone_axis_length = 0.1
      editors/3d_gizmos/gizmo_settings/bone_shape = 1
      editors/3d_gizmos/gizmo_colors/csg = Color(0, 0.4, 1, 0.15)
      editors/grid_map/editor_side = 1
      editors/grid_map/palette_min_width = 230
      editors/grid_map/preview_size = 64
      export/ssh/ssh = ""
      export/ssh/scp = ""
      network/language_server/remote_host = "127.0.0.1"
      network/language_server/remote_port = 6005
      network/language_server/enable_smart_resolve = true
      network/language_server/show_native_symbols_in_editor = false
      network/language_server/use_thread = false
      network/language_server/poll_limit_usec = 100000
      text_editor/theme/highlighting/gdscript/function_definition_color = Color(0.4, 0.9, 1, 1)
      text_editor/theme/highlighting/gdscript/global_function_color = Color(0.64, 0.64, 0.96, 1)
      text_editor/theme/highlighting/gdscript/node_path_color = Color(0.72, 0.77, 0.49, 1)
      text_editor/theme/highlighting/gdscript/node_reference_color = Color(0.39, 0.76, 0.35, 1)
      text_editor/theme/highlighting/gdscript/annotation_color = Color(1, 0.7, 0.45, 1)
      text_editor/theme/highlighting/gdscript/string_name_color = Color(1, 0.76, 0.65, 1)
      text_editor/theme/highlighting/comment_markers/critical_color = Color(0.77, 0.35, 0.35, 1)
      text_editor/theme/highlighting/comment_markers/warning_color = Color(0.72, 0.61, 0.48, 1)
      text_editor/theme/highlighting/comment_markers/notice_color = Color(0.56, 0.67, 0.51, 1)
      text_editor/theme/highlighting/comment_markers/critical_list = "ALERT,ATTENTION,CAUTION,CRITICAL,DANGER,SECURITY"
      text_editor/theme/highlighting/comment_markers/warning_list = "BUG,DEPRECATED,FIXME,HACK,TASK,TBD,TODO,WARNING"
      text_editor/theme/highlighting/comment_markers/notice_list = "INFO,NOTE,NOTICE,TEST,TESTING"
      editors/3d_gizmos/gizmo_colors/camera = Color(0.8, 0.4, 0.8, 1)
      editors/3d_gizmos/gizmo_colors/stream_player_3d = Color(0.4, 0.8, 1, 1)
      editors/3d_gizmos/gizmo_colors/occluder = Color(0.8, 0.5, 1, 1)
      editors/3d_gizmos/gizmo_colors/visibility_notifier = Color(0.8, 0.5, 0.7, 1)
      editors/3d_gizmos/gizmo_colors/particles = Color(0.8, 0.7, 0.4, 1)
      editors/3d_gizmos/gizmo_colors/particle_attractor = Color(1, 0.7, 0.5, 1)
      editors/3d_gizmos/gizmo_colors/particle_collision = Color(0.5, 0.7, 1, 1)
      editors/3d_gizmos/gizmo_colors/reflection_probe = Color(0.6, 1, 0.5, 1)
      editors/3d_gizmos/gizmo_colors/decal = Color(0.6, 0.5, 1, 1)
      editors/3d_gizmos/gizmo_colors/voxel_gi = Color(0.5, 1, 0.6, 1)
      editors/3d_gizmos/gizmo_colors/lightmap_lines = Color(0.5, 0.6, 1, 1)
      editors/3d_gizmos/gizmo_colors/lightprobe_lines = Color(0.5, 0.6, 1, 1)
      editors/3d_gizmos/gizmo_colors/joint_body_a = Color(0.6, 0.8, 1, 1)
      editors/3d_gizmos/gizmo_colors/joint_body_b = Color(0.6, 0.9, 1, 1)
      editors/3d_gizmos/gizmo_colors/fog_volume = Color(0.5, 0.7, 1, 1)
      text_editor/help/sort_functions_alphabetically = true
      metadata/script_setup_templates_dictionary = {
      "AnimatedSprite2D": "0NodeDefault",
      "Area2D": "0NodeDefault",
      "CanvasLayer": "0NodeDefault",
      "Node": "0NodeDefault",
      "Node2D": "0NodeDefault"
      }
      metadata/export_template_download_directory = "/home/emmet/.cache/godot"
    '';
  };
}
