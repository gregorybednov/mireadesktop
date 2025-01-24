{
    description = "MIREA desktop";
    inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    outputs = { self, nixpkgs, ... }:
        let
            pkgs = import nixpkgs { system = "x86_64-linux"; };
            wallpaper = builtins.readFile "${self}/wallpaper.svg";
            folderIcon = builtins.readFile "${self}/folderIcon.svg"; 
            pcmanfmConf = "${self}/pcmanfm.conf";
            desktopItems0 = (pkgs.writeText "desktop-items-0.conf"
''
[*]
wallpaper_mode=center
wallpaper_common=1
wallpapers_configured=1
wallpaper=${wallpaper}
desktop_bg=#77767b
desktop_fg=#ffffff
desktop_shadow=#000000
desktop_font=Sans 12
folder=/home/student
show_wm_menu=1
sort=mtime;ascending;
show_documents=0
show_trash=1
show_mounts=1
'');
            tint2conf1 = builtins.readFile "${self}/tint2conf.1";
            tint2conf2 = builtins.readFile "${self}/tint2conf.2";
            tint2config = pkgs.writeText "tint2conf"
''
${tint2conf1}
#-------------------------------------
button = new
button_lclick_command = ${jgmenu_run_prepared}
button_icon = ${self}/wallpaper.svg

separator = new
separator_style = empty
separator_padding = 3 4

button = new
button_lclick_command = ${pkgs.chromium}/bin/chromium
button_icon = ${pkgs.chromium}/share/icons/hicolor/48x48/apps/chromium.png

button = new
button_lclick_command = ${pkgs.pcmanfm}/bin/pcmanfm
button_icon = ${self}/folderIcon.svg

separator = new
separator_style = empty
separator_padding = 3 4

#-------------------------------------
execp = new
execp_name = switch
execp_command = ${pkgs.xkb-switch}/bin/xkb-switch
execp_interval = 1
execp_font = mono 10
execp_font_color = #dddddd 100
execp_centered = 1
execp_has_icon = 0
execp_padding = 5 5

#-------------------------------------
execp = new
execp_name = mireaweek
execp_command = ${mireaweek}/bin/mireaweek
execp_interval = 0
execp_font = mono 10
execp_font_color = #dddddd 100
execp_centered = 1
execp_has_icon = 0
execp_padding = 5 5

${tint2conf2}
'';
        jgmenu_run_prepared = 
        (pkgs.writeShellScript "jgmenu_run_prepared"
        ''${pkgs.jgmenu}/bin/jgmenu_run apps | ${prepare_jgmenu}/bin/prepare_jgmenu | ${pkgs.jgmenu}/bin/jgmenu --simple'');
        powermenu = (pkgs.writeShellScript "powermenu"
''
if [ "$1" = "poweroff" ]; then
	${pkgs.zenity}/bin/zenity --question --text "Уверены, что хотите выключить?" --default-cancel && poweroff
	exit 0
fi

if [ "$1" = "reboot" ]; then
	${pkgs.zenity}/bin/zenity --question --text "Уверены, что хотите перезагрузить?" --default-cancel && reboot
	exit 0
fi
'');
    mireaweek = pkgs.stdenv.mkDerivation rec {
            pname = "mireaweek";
            version = "0.1.0";
            src = ./weekday.kt;
	    dontInstall = true;
	    dontUnpack = true;
	    nativeBuildInputs = [pkgs.kotlin];
	    buildPhase = ''
		mkdir -p $out/{bin,lib}
		kotlinc $src -include-runtime -d mireaweek.jar
                mv mireaweek.jar $out/lib
                printf "${pkgs.jre}/bin/java -jar ../lib/mireaweek.jar" > $out/bin/mireaweek
		chmod +x $out/bin/mireaweek
	    '';
      
    };
    prepare_jgmenu = pkgs.stdenv.mkDerivation rec {
	    pname = "prepare_jgmenu";
	    version = "0.1.0";
	    src = ./JgmenuCorrector.kt;
	    dontInstall = true;
	    dontUnpack = true;
	    nativeBuildInputs = [pkgs.kotlin];
	    buildPhase = ''
		mkdir -p $out/{bin,lib}
		kotlinc $src -include-runtime -d prepare.jar
		mv prepare.jar $out/lib
                printf "${pkgs.jre}/bin/java -jar ../lib/prepare.jar" > $out/bin/prepare_jgmenu
		chmod +x $out/bin/prepare_jgmenu
	    '';
    };
    myxinitrc = pkgs.writeText ".xinitrc" "${tint2custom} &\n${pcmanfmdesktop} &\nexec ${pkgs.metacity}/bin/metacity";
    startmireadesktop = pkgs.writeShellScript "startmireadesktop"
    ''
        if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
            mkdir -p "$HOME/.config/pcmanfm/default"
            rm -rf "$HOME/.config/pcmanfm/default/*"
            cp ${desktopItems0} "$HOME/.config/default/desktop-items-0.conf"
            cp ${pcmanfmConf} "$HOME/.config/pcmanfm/default/pcmanfm.conf"
            chmod +w "$HOME/.config/pcmanfm/default/*"
            mkdir -p "$HOME/.config/gtk-3.0"
            printf "[Settings]\ngtk-icon-theme-name = WhiteSur" > "$HOME/.config/gtk-3.0/settings.ini"
            XINITRC=${myxinitrc} startx
        fi
    '';
tint2custom = pkgs.writeShellScript "tint2"
''
${pkgs.tint2}/bin/tint2 -c ${tint2config}
'';
pcmanfmdesktop = pkgs.writeShellScript "pcmanfm"
''
${pkgs.pcmanfm}/bin/pcmanfm --desktop 
'';
in {
    packages.x86_64-linux.powermenu = powermenu; # must be imported into configuration.nix!
    packages.x86_64-linux.tint2 = tint2custom;
    packages.x86_64-linux.pcmanfm = pcmanfmdesktop;
    packages.x86_64-linux.startmireadesktop = startmireadesktop;
    defaultPackage.x86_64-linux = startmireadesktop;
};
}
