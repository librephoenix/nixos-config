{ config, lib, pkgs, ... }:
let
  cfg = config.userSettings.qutebrowser;
  font = config.stylix.fonts.monospace.name;
  generateHomepage = name: font: config:
    ''<!DOCTYPE html>
      <html>

      <head>
        <title>My Local Dashboard Awesome Homepage</title>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <style>
          /*body*/
          body {
              background-color: #''+config.lib.stylix.colors.base00+''

          }
          /*paragraphs*/
          a, p {
              font-family:''+font+'';

              font-size:24px;
              text-align:center;
              color: #''+config.lib.stylix.colors.base08+'';

              line-height: 1.35;
              margin-top: 0;
              margin-bottom: 0;

              text-decoration: none;
          }
          a:hover {
              color: #''+config.lib.stylix.colors.base07+'';
          }
          .open {
            color: #''+config.lib.stylix.colors.base09+'';
            font-weight: bold;
          }
          .open:hover {
            background-color: #''+config.lib.stylix.colors.base09+'';
          }
          .quickmarks {
            color: #''+config.lib.stylix.colors.base0A+'';
            font-weight: bold;
          }
          .quickmarks:hover {
            background-color: #''+config.lib.stylix.colors.base0A+'';
          }
          .history {
            color: #''+config.lib.stylix.colors.base0B+'';
            font-weight: bold;
          }
          .history:hover {
            background-color: #''+config.lib.stylix.colors.base0B+'';
          }
          .newtab {
            color: #''+config.lib.stylix.colors.base0C+'';
            font-weight: bold;
          }
          .newtab:hover {
            background-color: #''+config.lib.stylix.colors.base0C+'';
          }
          .close {
            color: #''+config.lib.stylix.colors.base0D+'';
            font-weight: bold;
          }
          .close:hover {
            background-color: #''+config.lib.stylix.colors.base0D+'';
          }

          /*div*/
          div {
              margin:auto;
              width:50%;
              text-align:center;
          }
          /*class made for ascii art icon*/
          .icon {
              line-height:10%
          }
        </style>
      </head>

      <body>
        <!--start with cool qutebrowser ascii art-->
        <br>
        <br>
        <br>
        <div class="icon">
             <img width="300" src="logo.png">
        </div>
        <br>
        <br>
        <br>
        <!--qutebrowser title-->
        <p style="color:#''+config.lib.stylix.colors.base01+''">Welcome to Qutebrowser</p>
        <br>
        <p><b>''+name+" "+''Profile</b></p>
        <br>
        <!--basic keyboard commands-->
        <div>
          <a class="open" href="https://www.startpage.com"> [o] [Search] </p>
          <a class="quickmarks" href="./quickmarks.html" target="_blank" rel="noopener noreferrer"> [b] [Quickmarks] </p>
          <a class="newtab" href="./qute-home.html" target="_blank" rel="noopener noreferrer"> [t] [New tab] </p>
          <a class="close" href="#" onclick="window.close();return false;"> [x] [Close tab] </p>
        </div>
      </body>

      </html>
    '';
  qute-containers = ({ lib, stdenv, fetchFromGitHub, dmenuCmd ? config.userSettings.dmenuScripts.dmenuCmd, ... }:
                           let name = "qute-containers";
                               version = "unstable";
                               dmenu = dmenuCmd;
                           in
                             stdenv.mkDerivation {
                               inherit name version;
                           
                               src = fetchFromGitHub {
                                 owner = "s-praveen-kumar";
                                 repo = "qute-containers";
                                 rev = "c6164b94104fa8565200b87bfc87a2e08ca15ac7";
                                 sha256 = "sha256-g684sPSEJTRSk2V8LVrQsNeRIYtaQueRpZeREWtmQKw=";
                               };
                           
                               phases = "installPhase";
                           
                               postPatch = ''sed -i "s/qutebrowser/qutebrowser --qt-flag ignore-gpu-blacklist --qt-flag enable-gpu-rasterization --qt-flag enable-native-gpu-memory-buffers --qt-flag enable-accelerated-2d-canvas --qt-flag num-raster-threads=4/g" container-open'';
                           
                               installPhase = ''
                                 mkdir -p $out $out/bin
                                 cp $src/* $out/bin
                                 sed -i 's/DMENU=\"rofi -dmenu\"/DMENU=\"''+dmenu+''\"/g' $out/bin/containers_config
                                 sed -i 's/DMENU_FLAGS//g' $out/bin/container-open
                               '';
                           
                               meta = {
                                 homepage = "https://github.com/s-praveen-kumar/qute-containers";
                                 description = "Browser Containers for Qutebrowser";
                                 license = lib.licenses.mit;
                                 maintainers = [];
                               };
                             });
in {
  options = {
    userSettings.qutebrowser = {
      enable = lib.mkEnableOption "Enable qutebrowser with my settings";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.qutebrowser
                      (pkgs.callPackage qute-containers {})
                    ];

    home.file.".config/qutebrowser/userscripts/container-open".source = "${(pkgs.callPackage qute-containers {})}/bin/container-open";
    home.file.".config/qutebrowser/userscripts/containers_config".source = "${(pkgs.callPackage qute-containers {})}/bin/containers_config";
  
    programs.qutebrowser.enable = true;
    programs.qutebrowser.extraConfig = ''
import sys
import os.path
secretsExists = False
secretFile = os.path.expanduser("~/.config/qutebrowser/qutesecrets.py")

if (os.path.isfile(secretFile)):
    sys.path.append(os.path.dirname(secretFile))
    import qutesecrets
    secretsExists = True

quickmarksFile = os.path.join(os.path.dirname(__file__),'quickmarks')
quickmarksHtmlFilePath = os.path.join(os.path.dirname(__file__),'quickmarks.html')
quickmarksHtmlFile = open(quickmarksHtmlFilePath,"w")
quickmarksHtmlFileText = '<!DOCTYPE html><html><head><title>My Local Dashboard Awesome Homepage</title><meta name="viewport" content="width=device-width, initial-scale=1"><style>/*body*/body {background-color: #''+config.lib.stylix.colors.base00+''}/*paragraphs*/a, p {font-weight: bold;font-family:''+font+'';font-size:24px;text-align:center;color: #''+config.lib.stylix.colors.base07+'';line-height: 1.35;margin-top: 0;margin-bottom: 0;text-decoration: none;}a:hover {color: #''+config.lib.stylix.colors.base07+'';background-color: #''+config.lib.stylix.colors.base0A+'';}/*div*/div {margin:auto;width:50%;text-align:center;}/*class made for ascii art icon*/.icon {line-height:10%}.title{color: #''+config.lib.stylix.colors.base0A+'';text-decoration:underline;}</style></head><body><!--start with cool qutebrowser ascii art--><br><br><div class="icon"><img width="100" src="logo.png"></div><br><br><!--qutebrowser title--><p class="title">Quickmarks</p><br><div>' 
with open(quickmarksFile) as myQuickmarks:
    for line in myQuickmarks:
        quickmarkData = line.split()
        quickmarksHtmlFileText += '<a href="'+quickmarkData[1]+'">'+quickmarkData[0]+'</a><br>'
quickmarksHtmlFileText += '</div></body></html>'
quickmarksHtmlFile.write(quickmarksHtmlFileText)
quickmarksHtmlFile.close()

config.set('content.blocking.method','both')

config.set('scrolling.smooth',True)
config.set('qt.args',['ignore-gpu-blacklist','enable-gpu-rasterization','enable-accelerated-video-decode','enable-quic','enable-zero-copy','enable-native-gpu-memory-buffers','num-raster-threads=4','allow-file-access-from-files','enable-features=VaapiVideoDecoder,VaapiIgnoreDriverChecks,Vulkan,DefaultANGLEVulkan,VulkanFromANGLE'])
config.set('qt.workarounds.disable_accelerated_2d_canvas','never')
config.load_autoconfig(True)

base00 = "#''+config.lib.stylix.colors.base00+''"
base01 = "#''+config.lib.stylix.colors.base01+''"
base02 = "#''+config.lib.stylix.colors.base02+''"
base03 = "#''+config.lib.stylix.colors.base03+''"
base04 = "#''+config.lib.stylix.colors.base04+''"
base05 = "#''+config.lib.stylix.colors.base05+''"
base06 = "#''+config.lib.stylix.colors.base06+''"
base07 = "#''+config.lib.stylix.colors.base07+''"
base08 = "#''+config.lib.stylix.colors.base08+''"
base09 = "#''+config.lib.stylix.colors.base09+''"
base0A = "#''+config.lib.stylix.colors.base0A+''"
base0B = "#''+config.lib.stylix.colors.base0B+''"
base0C = "#''+config.lib.stylix.colors.base0C+''"
base0D = "#''+config.lib.stylix.colors.base0D+''"
base0E = "#''+config.lib.stylix.colors.base0E+''"
base0F = "#''+config.lib.stylix.colors.base0F+''"

config.set('content.cookies.accept', 'no-3rdparty', 'chrome-devtools://*')
config.set('content.cookies.accept', 'no-3rdparty', 'devtools://*')

config.set('content.headers.user_agent', 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')

config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')

config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

config.set('content.javascript.enabled', True, 'qute://*/*')

c.tabs.favicons.scale = 1.0
c.tabs.last_close = 'close'
c.tabs.position = 'left'
c.tabs.width = '3%'
c.window.transparent = True
c.colors.webpage.darkmode.enabled = ''+(if (config.stylix.polarity == "dark") then "True" else "False")+''

c.colors.webpage.preferred_color_scheme = "''+config.stylix.polarity+''"
c.colors.webpage.darkmode.policy.images = 'never'

c.url.default_page = str(config.configdir)+'/qute-home.html'
c.url.start_pages = str(config.configdir)+'/qute-home.html'

c.url.searchengines = {'DEFAULT': 'https://search.brave.com/search?q={}&source=web',
                       'sp': 'https://startpage.com/do/search?query={}',
                       'd'      : 'https://duckduckgo.com/?q={}&ia=web',
                       'az'     : 'https://www.amazon.com/s?k={}',
                       'aw'     : 'https://wiki.archlinux.org/index.php?search={}&title=Special%3ASearch&wprov=acrw1',
                       'nw'     : 'https://nixos.wiki/index.php?search={}&go=Go',
                       'mn'     : 'https://mynixos.com/search?q={}',
                       'sb'     : 'https://www.serebii.net/search.shtml?q={}&sa=Search',
                       'bp'     : 'https://bulbapedia.bulbagarden.net/wiki/index.php?title=Special%3ASearch&search={}&go=Go',
                       'yt'     : 'https://www.youtube.com/results?search_query={}',
                       'od'     : 'https://odysee.com/$/search?q={}',
                       'gd'     : 'https://drive.google.com/drive/search?q={}',
                       'gh'     : 'https://github.com/search?q={}&type=repositories',
                       'gl'     : 'https://gitlab.com/search?search={}&nav_source=navbar',
                       'np'     : 'https://github.com/search?q=repo%3ANixOS%2Fnixpkgs%20{}&type=code',
                       'wk'     : 'https://en.wikipedia.org/w/index.php?fulltext=1&search={}&title=Special%3ASearch&ns0=1',
                       'th'     : 'https://www.thingiverse.com/search?q={}&page=1',
                       'dh'     : 'https://hub.docker.com/search?q={}'
                      }

config.set('completion.open_categories',["searchengines","quickmarks","bookmarks"])

config.set('downloads.location.directory', '~/Downloads')
config.set('downloads.location.prompt', False)
config.set('downloads.position', 'bottom')
config.set('downloads.remove_finished', 5000)

config.set('fileselect.handler', 'external')
config.set('fileselect.single_file.command', ['kitty','--class','filechoose_yazi','-e','yazi','--chooser-file={}'])
config.set('fileselect.multiple_files.command', ['kitty','--class','filechoose_yazi','-e','yazi','--chooser-file={}'])
config.set('fileselect.folder.command', ['kitty','--class','--filechoose_yazi','-e','yazi','--chooser-file={}']) # needs fixing upstream to work

# bindings from doom emacs
config.bind('<Alt-x>', 'cmd-set-text :')
config.bind('<Space>.', 'cmd-set-text :')
config.bind('<Space>b', 'bookmark-list')
config.bind('<Space>h', 'history')
config.bind('<Space>gh', 'open https://github.com')
config.bind('<Space>gl', 'open https://gitlab.com')
config.bind('<Space>gc', 'open https://codeberg.org')
if (secretsExists):
    config.bind('<Space>gg', 'open '+qutesecrets.mygiteadomain)
config.bind('<Ctrl-p>', 'completion-item-focus prev', mode='command')
config.bind('<Ctrl-n>', 'completion-item-focus next', mode='command')
config.bind('<Ctrl-p>', 'fake-key <Up>', mode='normal')
config.bind('<Ctrl-n>', 'fake-key <Down>', mode='normal')
config.bind('<Ctrl-p>', 'fake-key <Up>', mode='insert')
config.bind('<Ctrl-n>', 'fake-key <Down>', mode='insert')
config.bind('<Ctrl-p>', 'fake-key <Up>', mode='passthrough')
config.bind('<Ctrl-n>', 'fake-key <Down>', mode='passthrough')

# bindings from vimium
config.bind('t', 'open -t')
config.bind('x', 'tab-close')
config.bind('yf', 'hint links yank')
config.bind('<Ctrl-Tab>', 'tab-next')
config.bind('<Ctrl-Shift-Tab>', 'tab-prev')

# passthrough bindings
config.bind('<Shift-Escape>', 'mode-leave', mode='passthrough')
config.bind('<Ctrl-T>', 'open -t', mode='passthrough')
config.bind('<Ctrl-W>', 'tab-close', mode='passthrough')
config.bind('<Ctrl-Tab>', 'tab-next', mode='passthrough')
config.bind('<Ctrl-Shift-Tab>', 'tab-prev', mode='passthrough')
config.bind('<Ctrl-B>', 'cmd-set-text -s :quickmark-load -t', mode='passthrough')
config.bind('<Ctrl-O>', 'cmd-set-text -s :open -t', mode='passthrough')
config.bind('<Ctrl-F>', 'cmd-set-text /', mode='passthrough')
config.bind('<Ctrl-R>', 'reload', mode='passthrough')
config.unbind('<Ctrl-X>')
config.unbind('<Ctrl-A>')

# spawn external programs
config.bind(',m', 'hint links spawn mpv {hint-url}')
config.bind(',co', 'spawn container-open')
config.bind(',cf', 'hint links userscript container-open')

# current_stylesheet_directory = '~/.config/qutebrowser/themes/'
# current_stylesheet = base16_theme+'-all-sites.css'
# current_stylesheet_path = current_stylesheet_directory + current_stylesheet
# config.set('content.user_stylesheets', current_stylesheet_path)
#config.bind(',s', 'set content.user_stylesheets \'\' ')
#config.bind(',S', 'set content.user_stylesheets '+current_stylesheet_path)

# theming
c.colors.completion.fg = base05
c.colors.completion.odd.bg = base01
c.colors.completion.even.bg = base00
c.colors.completion.category.fg = base0A
c.colors.completion.category.bg = base00
c.colors.completion.category.border.top = base00
c.colors.completion.category.border.bottom = base00
c.colors.completion.item.selected.fg = base05
c.colors.completion.item.selected.bg = base02
c.colors.completion.item.selected.border.top = base02
c.colors.completion.item.selected.border.bottom = base02
c.colors.completion.item.selected.match.fg = base0B
c.colors.completion.match.fg = base0B
c.colors.completion.scrollbar.fg = base05
c.colors.completion.scrollbar.bg = base00
c.colors.contextmenu.disabled.bg = base01
c.colors.contextmenu.disabled.fg = base04
c.colors.contextmenu.menu.bg = base00
c.colors.contextmenu.menu.fg =  base05
c.colors.contextmenu.selected.bg = base02
c.colors.contextmenu.selected.fg = base05
c.colors.downloads.bar.bg = base00
c.colors.downloads.start.fg = base00
c.colors.downloads.start.bg = base0D
c.colors.downloads.stop.fg = base00
c.colors.downloads.stop.bg = base0C
c.colors.downloads.error.fg = base08
c.colors.hints.fg = base00
c.colors.hints.bg = base0A
c.colors.hints.match.fg = base05
c.colors.keyhint.fg = base05
c.colors.keyhint.suffix.fg = base05
c.colors.keyhint.bg = base00
c.colors.messages.error.fg = base00
c.colors.messages.error.bg = base08
c.colors.messages.error.border = base08
c.colors.messages.warning.fg = base00
c.colors.messages.warning.bg = base0E
c.colors.messages.warning.border = base0E
c.colors.messages.info.fg = base05
c.colors.messages.info.bg = base00
c.colors.messages.info.border = base00
c.colors.prompts.fg = base05
c.colors.prompts.border = base00
c.colors.prompts.bg = base00
c.colors.prompts.selected.bg = base02
c.colors.prompts.selected.fg = base05
c.colors.statusbar.normal.fg = base0B
c.colors.statusbar.normal.bg = base00
c.colors.statusbar.insert.fg = base00
c.colors.statusbar.insert.bg = base0D
c.colors.statusbar.passthrough.fg = base00
c.colors.statusbar.passthrough.bg = base0C
c.colors.statusbar.private.fg = base00
c.colors.statusbar.private.bg = base01
c.colors.statusbar.command.fg = base05
c.colors.statusbar.command.bg = base00
c.colors.statusbar.command.private.fg = base05
c.colors.statusbar.command.private.bg = base00
c.colors.statusbar.caret.fg = base00
c.colors.statusbar.caret.bg = base0E
c.colors.statusbar.caret.selection.fg = base00
c.colors.statusbar.caret.selection.bg = base0D
c.colors.statusbar.progress.bg = base0D
c.colors.statusbar.url.fg = base05
c.colors.statusbar.url.error.fg = base08
c.colors.statusbar.url.hover.fg = base05
c.colors.statusbar.url.success.http.fg = base0C
c.colors.statusbar.url.success.https.fg = base0B
c.colors.statusbar.url.warn.fg = base0E
c.colors.tabs.bar.bg = base00
c.colors.tabs.indicator.start = base0D
c.colors.tabs.indicator.stop = base0C
c.colors.tabs.indicator.error = base08
c.colors.tabs.odd.fg = base05
c.colors.tabs.odd.bg = base01
c.colors.tabs.even.fg = base05
c.colors.tabs.even.bg = base00
c.colors.tabs.pinned.even.bg = base0C
c.colors.tabs.pinned.even.fg = base07
c.colors.tabs.pinned.odd.bg = base0B
c.colors.tabs.pinned.odd.fg = base07
c.colors.tabs.pinned.selected.even.bg = base02
c.colors.tabs.pinned.selected.even.fg = base05
c.colors.tabs.pinned.selected.odd.bg = base02
c.colors.tabs.pinned.selected.odd.fg = base05
c.colors.tabs.selected.odd.fg = base05
c.colors.tabs.selected.odd.bg = base02
c.colors.tabs.selected.even.fg = base05
c.colors.tabs.selected.even.bg = base02

font = "''+font+''"

c.fonts.default_family = font
c.fonts.default_size = '14pt'

c.fonts.web.family.standard = font
c.fonts.web.family.serif = font
c.fonts.web.family.sans_serif = font
c.fonts.web.family.fixed = font
c.fonts.web.family.fantasy = font
c.fonts.web.family.cursive = font
    '';
  
    home.file.".config/qutebrowser/containers".text = ''
Gamedev
Teaching
Youtube
    '';
  
    home.file.".config/qutebrowser/qute-home.html".text = generateHomepage "Default" font config;
    home.file.".config/qutebrowser/logo.png".source = ./qutebrowser-logo.png;
    home.file.".browser/Gamedev/config/qute-home.html".text = generateHomepage "Gamedev" font config;
    home.file.".browser/Gamedev/config/logo.png".source = ./qutebrowser-logo.png;
    home.file.".browser/Teaching/config/qute-home.html".text = generateHomepage "Teaching" font config;
    home.file.".browser/Teaching/config/logo.png".source = ./qutebrowser-logo.png;
    home.file.".browser/Youtube/config/qute-home.html".text = generateHomepage "Youtube" font config;
    home.file.".browser/Youtube/config/logo.png".source = ./qutebrowser-logo.png;

    xdg.mimeApps.defaultApplications = lib.mkIf (config.userSettings.browser == "qutebrowser" ) {
        "text/html" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/http" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/https" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/about" = "org.qutebrowser.qutebrowser.desktop";
        "x-scheme-handler/unknown" = "org.qutebrowser.qutebrowser.desktop";
    };

    home.sessionVariables = lib.mkIf (config.userSettings.browser == "qutebrowser") {
      DEFAULT_BROWSER = "${pkgs.qutebrowser}/bin/qutebrowser";
      QTWEBENGINE_CHROMIUM_FLAGS = "--disable-gpu"; # FIXME https://github.com/qutebrowser/qutebrowser/issues/8535
    };

  };
}
