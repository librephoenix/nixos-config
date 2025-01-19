import os
import bpy

# load prefs
prefs = bpy.context.preferences

# ui
prefs.view.ui_scale = 1.2
prefs.view.show_tooltips_python = True
prefs.view.render_display_type = 'SCREEN'
prefs.view.filebrowser_display_type = 'SCREEN'
prefs.view.gizmo_size_navigate_v3d = 50

# status bar
prefs.view.show_statusbar_stats = True
prefs.view.show_statusbar_scene_duration = True
prefs.view.show_statusbar_memory = True
prefs.view.show_statusbar_vram = True
prefs.view.color_picker_type = 'SQUARE_SV'

# performance
prefs.system.viewport_aa = 'FXAA'

# addons
bpy.ops.preferences.addon_enable(module="node_wrangler")
bpy.ops.preferences.addon_enable(module="rigify")
bpy.ops.preferences.addon_install(filepath=os.path.expanduser("~/.config/blender/extensions/node_pie.zip"),enable_on_install=True)
bpy.ops.extensions.package_install_files(filepath=os.path.expanduser("~/.config/blender/extensions/bool_tool.zip"),repo="user_default",enable_on_install=True)
