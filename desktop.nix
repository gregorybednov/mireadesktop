let
    wallpaper = builtins.readFile "./wallpaper.svg";
    folderIcon = builtins.readFile "./folderIcon.svg"; 
    pcmanfmConf = builtins.readFile "./pcmanfm.conf";
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
    tint2conf1 = builtins.readFile "./tint2conf.1";
    tint2conf2 = builtins.readFile "./tint2conf.2";
    tint2config = pkgs.writeText "tint2conf"
''
${tint2conf1}
#-------------------------------------
button = new
button_lclick_command = ${jgmenu_run_prepared}
button_icon = ${wallpaper}

separator = new
separator_style = empty
separator_padding = 3 4

button = new
button_lclick_command = ${pkgs.chromium}/bin/chromium
button_icon = ${pkgs.chromium}/share/icons/hicolor/48x48/apps/chromium.png

button = new
button_lclick_command = ${pkgs.pcmanfm}/bin/pcmanfm
button_icon = ${folderIcon}

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
execp_command = ${mireaweek}/bin/weekday
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
	    src = pkgs.writeTextFile {
		    name = "weekday.hs";
		    text = ''
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)

data Period = Autumn
            | Winter
            | Spring
            deriving (Show, Eq)

period :: (Ord a, Num t) => (MonthOfYear -> t -> a) -> a -> Period
period thisYear today
    | today < thisYear February 9 = Winter
    | today >= thisYear September 1 = Autumn
    | otherwise = Spring 

week :: Period -> Day -> String
week Winter _ = "Хороших праздников, удачной сессии!"
week p d
    | dayOfWeek d == Sunday = "Сегодня воскресенье, лучше иди домой"
    | otherwise = show x ++ " неделя" where
                        x = 1 + x0 - x1 - if limitIsSunday then 1 else 0
                        limitIsSunday = dayOfWeek limit == Sunday 
                        (_, x1, _) = toWeekDate limit
                        (y, x0, _) = toWeekDate d
                        limit = if p == Spring
                            then this February 9
                            else this September 1
                        this = fromGregorian y
main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    --let today = fromGregorian 2024 September 9 -- it was TEST
    let (year,_,_) = toGregorian today
    putStr $ week (period (fromGregorian year) today) today'';
	    };
	    dontInstall = true;
	    dontUnpack = true;
	    nativeBuildInputs = [pkgs.ghc];
	    buildPhase = ''
		mkdir -p $out/bin
		ghc $src -o $out/bin/weekday
	    '';
};
    tint2 = pkgs.writeScriptBin "tint2" "${pkgs.tint2}/bin/tint2 -c ${tint2config}";
    preparejgmenu = (pkgs.writeShellScript "preparejgmenu"
''
str=$(cat - | sed "/soffice --math/d; /soffice --draw/d; /startcenter/d; /soffice --base/d; /apps-dir-Settings/d; /tint2conf/d; /nvidia-settings/d; /--desktop-pref/d; /xterm/d; /jgmenu/d; /tint2/d;
s/,applications-system/,applications-system,\nВыключение...,^checkout(apps-dir-Powermenu),applications-powermenu/;
s/\^tag(apps-dir-Powermenu)/\^tag(apps-dir-Powermenu)\nArchi (Archimate Modeling Tool),Archi,Archi,,#Education/; 
s/,applications-office/,applcations-office\nБД и проек-ие,^checkout(apps-dir-Database),applications-database/;
/\^tag(apps-dir-Education)/d;
s/apps-dir-Other/apps-dir-Database/;
/Прочее/d;
s/\^tag(apps-dir-Programming)/\^tag(apps-dir-Programming)\nSimInTech,simintech,simintech,,#Education/;
s/\^tag(apps-dir-Database)/\^tag(apps-dir-Database)\nArchi (Archimate Modeling Tool),Archi,Archi,,#Education/; ")

printf "$str\n\n^tag(apps-dir-Powermenu)\nВыключить,${powermenu} poweroff,,,#System\nПерезагрузить,${powermenu} reboot,,,#System\n"
'');
    myxinitrc = pkgs.writeText ".xinitrc" "${tint2} &\n${pkgs.pcmanfm}/bin/pcmanfm --desktop &\nexec ${pkgs.metacity}";
in
    myxinitrc;