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
#execp_command = $ mireaweek}/bin/weekday # TODO!!!
execp_interval = 0
execp_font = mono 10
execp_font_color = #dddddd 100
execp_centered = 1
execp_has_icon = 0
execp_padding = 5 5

${tint2conf2}
'';
        jgmenu_run_prepared = 
        (pkgs.writeShellScript "jgmenu_run_prepared" ''
        ${pkgs.jgmenu}/bin/jgmenu_run apps | ${preparejgmenu} | ${pkgs.jgmenu}/bin/jgmenu --simple
'');
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
	    version = "0.2.0";
	    src = ./weekday.hs;
	    dontInstall = true;
	    dontUnpack = true;
	    nativeBuildInputs = [pkgs.ghc];
	    buildPhase = ''
		mkdir -p $out/bin
		ghc $src -o $out/bin/weekday
	    '';
};
    preparejgmenu = (pkgs.writeShellScript "preparejgmenu"
''
str=$(cat - | sed "/soffice --math/d; /soffice --draw/d; /IntelliJ IDEA CE/; /startcenter/d; /soffice --base/d; /apps-dir-Settings/d; /tint2conf/d; /nvidia-settings/d; /--desktop-pref/d; /xterm/d; /jgmenu/d; /tint2/d;
s/,applications-system/,applications-system,\nВыключение...,^checkout(apps-dir-Powermenu),applications-powermenu/;
s/\^tag(apps-dir-Powermenu)/\^tag(apps-dir-Powermenu)\nArchi (Archimate Modeling Tool),Archi,Archi,,#Education/; 
s/(Free Java.*)//;
s/IntelliJ IDEA Community//; 
s/,applications-office/,applcations-office\nБД и проек-ие,^checkout(apps-dir-Database),applications-database/;
/\^tag(apps-dir-Education)/d;
s/apps-dir-Other/apps-dir-Database/;
/Прочее/d;
s/\^tag(apps-dir-Programming)/\^tag(apps-dir-Programming)\nSimInTech,simintech,simintech,,#Education/;
s/\^tag(apps-dir-Database)/\^tag(apps-dir-Database)\nArchi (Archimate Modeling Tool),Archi,Archi,,#Education/; ")

printf "$str\n\n^tag(apps-dir-Powermenu)\nВыключить,${powermenu} poweroff,,,#System\nПерезагрузить,${powermenu} reboot,,,#System\n"
'');
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
    packages.x86_64-linux.tint2 = tint2custom;
    packages.x86_64-linux.pcmanfm = pcmanfmdesktop;
    packages.x86_64-linux.startmireadesktop = startmireadesktop;
    defaultPackage.x86_64-linux = startmireadesktop;
};
}
