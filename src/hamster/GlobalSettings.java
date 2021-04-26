package hamster;

import hamster.data.*;
import hamster.data.food.FoodData;
import hamster.data.gob.ObjData;
import hamster.data.itm.ItemData;
import hamster.data.map.MarkerData;
import hamster.gob.Alerted;
import hamster.gob.Deleted;
import hamster.gob.Hidden;
import hamster.script.LispScript;
import hamster.ui.chr.CredoTree;
import hamster.ui.chr.SkillTree;
import hamster.util.JobSystem;
import haven.Coord;
import haven.Indir;
import haven.JOGLPanel;
import haven.render.BaseColor;

import java.awt.*;

/**
 * A list of settings that will work across all sessions
 */
public class GlobalSettings {
    private static final Settings global = new Settings("global");
    private static final Settings tmp = new Settings("gtmp", false);

    public static void init() {
        //preload lisp scripting config
        JobSystem.submit(LispScript::reloadConfig);
        ObjData.init();
        ItemData.init();
        MarkerData.init();
        Alerted.init();
        CredoTree.init();
        SkillTree.init();
        Deleted.init();
        Hidden.init();
        HighlightData.init();
        MenuExclusionData.init();
        ShortenData.init();
        FoodData.init();
    }

    //Non-saved globals
    public static final IndirSetting<Boolean> PAUSED = new IndirSetting<>(tmp, "tmp.pause", false);
    public static final IndirSetting<Boolean> GENERATINGTOKEN = new IndirSetting<>(tmp, "generate-token", false);
    public static final IndirSetting<Boolean> SHOWHIDDEN = new IndirSetting<>(tmp, "session.show-hidden", false);
    public static final IndirSetting<Boolean> SHOWHITBOX = new IndirSetting<>(tmp, "session.show-hitbox", false);
    public static final IndirSetting<Boolean> SHOWHOVERTOOLTIPS = new IndirSetting<>(tmp, "session.show-hover-tooltips", false);
    public static final IndirSetting<Boolean> SHOWPCLAIM = new IndirSetting<>(tmp, "session.show-pclaim", false);
    public static final IndirSetting<Boolean> SHOWVCLAIM = new IndirSetting<>(tmp, "session.show-vclaim", false);
    public static final IndirSetting<Boolean> SHOWKCLAIM = new IndirSetting<>(tmp, "session.show-kclaim", false);

    //Internal settings
    public static final IndirSetting<Coord> WINDOWPOS = new IndirSetting<>(global, "intenral.window-pos", Coord.z);

    //General options
    public static final IndirSetting<Boolean> DEBUG = new IndirSetting<>(global, "system.debug", false);
    public static final IndirSetting<Boolean> SHOWFPS = new IndirSetting<>(global, "system.show-fps", false);
    public static final IndirSetting<Boolean> SHOWSTATS = new IndirSetting<>(global, "system.show-stats", false);

    //Audio options
    public static final IndirSetting<Integer> MASTERVOL = new IndirSetting<>(global, "audio.master-volume", 1000);
    public static final IndirSetting<Integer> EVENTVOL = new IndirSetting<>(global, "audio.event-volume", 1000);
    public static final IndirSetting<Integer> AMBIENTVOL = new IndirSetting<>(global, "audio.ambient-volume", 1000);
    public static final IndirSetting<Integer> UIVOL = new IndirSetting<>(global, "audio.overall-ui-volume", 1000);
    public static final IndirSetting<Integer> TIMERVOL = new IndirSetting<>(global,"audio.timer-volume", 1000);
    public static final IndirSetting<Integer> ALERTVOL = new IndirSetting<>(global, "audio.alert-volume", 1000);
    public static final IndirSetting<Integer> POPUPMSGVOL = new IndirSetting<>(global, "audio.popup-message-volume", 1000);
    public static final IndirSetting<Integer> ERRORMSGVOL = new IndirSetting<>(global, "audio.error-message-volume", 1000);
    public static final IndirSetting<Integer> COMBATSTARTVOL = new IndirSetting<>(global, "audio.combat-start-volume", 1000);
    public static final IndirSetting<Boolean> COMBATSTARTAUDIO = new IndirSetting<>(global, "audio.combat-start", true);
    public static final IndirSetting<String> COMBATSTARTAUDIORES = new IndirSetting<>(global, "audio.combat-start-res", "custom/sfx/howl");

    public static final IndirSetting<Boolean> SOUNDONERRORMSG = new IndirSetting<>(global, "audio.sound-on-error", true);
    public static final IndirSetting<Boolean> SOUNDONPOPUPMSG = new IndirSetting<>(global, "audio.sound-on-popup", true);
    public static final IndirSetting<Boolean> SOUNDONGOBAUDIO = new IndirSetting<>(global, "audio.no-gob-audio", true);

    //Display options
    public static final IndirSetting<Boolean> VSYNC = new IndirSetting<>(global, "display.vsync", true);
    public static final IndirSetting<Integer> FPS = new IndirSetting<>(global, "display.fps", 60);
    public static final IndirSetting<Integer> BGFPS = new IndirSetting<>(global, "display.bg-fps", 5);
    public static final IndirSetting<Boolean> SHADOWS = new IndirSetting<>(global, "display.shadows.show", true);
    public static final IndirSetting<Integer> SHADOWQUALITY = new IndirSetting<>(global, "display.shadows.quality", 4);
    public static final IndirSetting<Integer> SHADOWSIZE = new IndirSetting<>(global, "display.shadows.size", 3);
    public static final IndirSetting<Integer> SHADOWDEPTH = new IndirSetting<>(global, "display.shadows.depth", 3);
    public static final IndirSetting<Boolean> SYMMETRICOUTLINES = new IndirSetting<>(global, "display.outlines.symmetric", false);
    public static final IndirSetting<Double> UISCALE = new IndirSetting<>(global, "display.ui-scale", 1.0);
    public static final IndirSetting<String> FRAMESYNCMODE = new IndirSetting<>(global, "display.frame-sync-mode", JOGLPanel.SyncMode.FRAME.name());
    public static final IndirSetting<Boolean> WIREFRAMEMODE = new IndirSetting<>(global, "display.wireframe-mode", false);

    // Map options
    public static final IndirSetting<Boolean> KEEPGOBS = new IndirSetting<>(global, "map.keep-gobs", false);
    public static final IndirSetting<Boolean> KEEPGRIDS = new IndirSetting<>(global, "map.keep-grids", false);
    public static final IndirSetting<Boolean> SKIPLOADING = new IndirSetting<>(global, "map.skip-loading", false);
    public static final IndirSetting<Integer> DRAWGRIDRADIUS = new IndirSetting<>(global, "map.drag-grid-radius", 2);
    public static final IndirSetting<Boolean> SHOWFLAVOBJS = new IndirSetting<>(global, "map.show-flav-objs", true);
    public static final IndirSetting<Boolean> SHOWMAP = new IndirSetting<>(global, "map.show-map", true);
    public static final IndirSetting<Boolean> SHOWGOBS = new IndirSetting<>(global, "map.show-gobs", true);
    public static final IndirSetting<Boolean> FLATWORLD = new IndirSetting<>(global, "map.flat-world", false);
    public static final IndirSetting<Boolean> SHOWTRANTILES = new IndirSetting<>(global, "map.show-tran-tiles", true);
    public static final IndirSetting<Boolean> SHOWWATERSURF = new IndirSetting<>(global, "map.show-water-surface", true);
    public static final IndirSetting<Boolean> COLORIZEDEEPWATER = new IndirSetting<>(global, "map.colorize-deep-ocean", true);
    public static final IndirSetting<Color> DEEPWATERCOL = new IndirSetting<>(global, "map.deep-ocean-color", new Color(128, 7, 7));
    public static final IndirSetting<Boolean> SHORTCAVEWALLS = new IndirSetting<>(global, "map.short-cave-walls", false);
    public static final IndirSetting<Boolean> LONGLIVINGDUST = new IndirSetting<>(global, "map.long-living-cave-dust", false);
    public static final IndirSetting<Boolean> COLORFULDUST = new IndirSetting<>(global, "map.colorful-cave-dust", false);
    public static final IndirSetting<Boolean> LARGEDUSTSIZE = new IndirSetting<>(global, "map.large-cave-dust", false);
    public static final IndirSetting<Boolean> SHOWSNOW =  new IndirSetting<>(global, "map.spawn-snowflakes", false);
    public static final IndirSetting<Boolean> LARGESNOWFLAKE =  new IndirSetting<>(global, "map.large-snowflakes", false);
    public static final IndirSetting<Integer> SNOWDENSITY =  new IndirSetting<>(global, "map.snow-density", 1);
    public static final IndirSetting<Integer> SNOWFALLSPEED =  new IndirSetting<>(global, "map.snow-speed", 1);
    public static final IndirSetting<Integer> SNOWGUSTSPEED =  new IndirSetting<>(global, "map.snow-gust-speed", 100);
    public static final IndirSetting<Boolean> SHOWWEATHER = new IndirSetting<>(global, "map.show-weather", true);
    public static final IndirSetting<Boolean> ALLOWSHAKING = new IndirSetting<>(global, "map.allow-shaking", true);

    //Minimap
    public static final IndirSetting<Boolean> SHOWMMGOBS = new IndirSetting<>(global, "minimap.show-gobs", true);
    public static final IndirSetting<Boolean> SHOWMMMARKERNAMES = new IndirSetting<>(global, "minimap.show-marker-names", true);
    public static final IndirSetting<Boolean> SHOWMMGOBNAMES = new IndirSetting<>(global, "minimap.show-gob-names", true);
    public static final IndirSetting<Boolean> SHOWMMMARKERS = new IndirSetting<>(global, "minimap.show-markers", true);
    public static final IndirSetting<Boolean> SMALLMMMARKERS = new IndirSetting<>(global, "minimap.show-small-markers", true);
    public static final IndirSetting<Boolean> SHOWPMARKERS = new IndirSetting<>(global, "minimap.show-placed-markers", true);
    public static final IndirSetting<Boolean> SHOWNMARKERS = new IndirSetting<>(global, "minimap.show-natural-markers", true);
    public static final IndirSetting<Boolean> SHOWCMARKERS = new IndirSetting<>(global, "minimap.show-custom-markers", true);
    public static final IndirSetting<Boolean> SHOWLMARKERS = new IndirSetting<>(global, "minimap.show-linked-markers", true);
    public static final IndirSetting<Boolean> SHOWKMARKERS = new IndirSetting<>(global, "minimap.show-kingdom-markers", true);
    public static final IndirSetting<Boolean> SHOWKMARKERRAD = new IndirSetting<>(global, "minimap.show-kingdom-radius", true);
    public static final IndirSetting<Boolean> SHOWVMARKERS = new IndirSetting<>(global, "minimap.show-village-markers", true);
    public static final IndirSetting<Boolean> SHOWVMARKERRAD = new IndirSetting<>(global, "minimap.show-village-radius", true);
    public static final IndirSetting<Boolean> SHOWVMARKERTIPS = new IndirSetting<>(global, "minimap.show-village-names", true);
    public static final IndirSetting<Coord> MMMEMSIZEONE = new IndirSetting<>(global, "minimap.mem-size-one", new Coord(100, 100));
    public static final IndirSetting<Coord> MMMEMPOSONE = new IndirSetting<>(global, "minimap.mem-pos-one", new Coord(500, 100));
    public static final IndirSetting<Coord> MMMEMSIZETWO = new IndirSetting<>(global, "minimap.mem-size-two", new Coord(300, 300));
    public static final IndirSetting<Coord> MMMEMPOSTWO = new IndirSetting<>(global, "minimap.mem-pos-two", new Coord(500, 100));
    public static final IndirSetting<Boolean> MMSHOWGRID = new IndirSetting<>(global, "minimap.show-grid", false);
    public static final IndirSetting<Boolean> MMSHOWVIEW = new IndirSetting<>(global, "minimap.show-view", true);
    public static final IndirSetting<Color> MMPATHCOL = new IndirSetting<>(global, "minimap.path-color", Color.magenta);

    //Gob
    public static final IndirSetting<Boolean> CIRCLEAGGRO = new IndirSetting<>(global, "gob.colorize-aggro", true);
    public static final IndirSetting<Boolean> CIRCLEFRIENDS = new IndirSetting<>(global, "gob.circle-friends", true);
    public static final IndirSetting<Integer> BADKIN = new IndirSetting<>(global, "gob.bad-kin-color", 2);
    public static final IndirSetting<Boolean> COLORFULFARMES = new IndirSetting<>(global, "gob.colorful-frames", true);
    public static final IndirSetting<Boolean> COLORFULTUBS = new IndirSetting<>(global, "gob.colorful-tubs", true);
    public static final IndirSetting<Boolean> COLORFULCUPBOARDS = new IndirSetting<>(global, "gob.colorful-cupboards", true);
    public static final IndirSetting<Boolean> COLORFULCHEESERACKS = new IndirSetting<>(global, "gob.colorful-cheese-racks", true);
    public static final IndirSetting<Boolean> SHOWCROPSTAGE = new IndirSetting<>(global, "gob.show-crop-stage", false);
    public static final IndirSetting<Boolean> SIMPLECROPS = new IndirSetting<>(global, "gob.simple-crops", false);
    public static final IndirSetting<BaseColor> GOBHIDDENCOL = new IndirSetting<>(global, "gob.hidden-col", new BaseColor(Color.WHITE));
    public static final IndirSetting<BaseColor> GOBHITBOXCOL = new IndirSetting<>(global, "gob.hitbox-col", new BaseColor(Color.WHITE));
    public static final IndirSetting<Boolean> SHOWGOBHALO = new IndirSetting<>(global, "gob.show-gob-halo", false);
    public static final IndirSetting<Boolean> SHOWPLAYERHALO = new IndirSetting<>(global, "gob.show-player-halo", false);
    public static final IndirSetting<Boolean> SHOWGOBHALOONHEARTH = new IndirSetting<>(global, "gob.show-gob-halo-on-hearth", true);
    public static final IndirSetting<Boolean> SHOWGOBHP = new IndirSetting<>(global, "gob.show-gob-hp", true);
    public static final IndirSetting<Integer> PATHWIDTH = new IndirSetting<>(global, "gob.path-width", 4);
    public static final IndirSetting<Boolean> SHOWGOBPATH = new IndirSetting<>(global, "gob.show-gob-path", false);
    public static final IndirSetting<Boolean> SHOWANIMALPATH = new IndirSetting<>(global, "gob.show-animal-path", false);
    public static final IndirSetting<Boolean> SHOWANIMALRADIUS = new IndirSetting<>(global, "gob.show-animal-radius", false);
    public static final IndirSetting<BaseColor> GOBPATHCOL = new IndirSetting<>(global, "gob.gob-path-color", new BaseColor(Color.GREEN));
    public static final IndirSetting<BaseColor> ANIMALPATHCOL = new IndirSetting<>(global, "gob.animal-path-color", new BaseColor(Color.RED));
    public static final IndirSetting<BaseColor> VEHPATHCOL = new IndirSetting<>(global, "gob.vehicle-path-color", new BaseColor(Color.ORANGE));
    public static final IndirSetting<Boolean> SHOWGOBSPEED = new IndirSetting<>(global, "gob.show-speed-on-humans", false);
    public static final IndirSetting<Boolean> SHOWANIMALSPEED = new IndirSetting<>(global, "gob.show-speed-on-animals", false);
    public static final IndirSetting<Boolean> SHOWPLAYERDMG = new IndirSetting<>(global, "gob.show-player-dmg", true);
    public static final IndirSetting<Boolean> SHOWANIMALDMG = new IndirSetting<>(global, "gob.show-animal-dmg", true);
    public static final IndirSetting<Boolean> SHORTWALLS = new IndirSetting<>(global, "gob.short-walls", false);
    public static final IndirSetting<Boolean> SHORTCUPBOARDS = new IndirSetting<>(global, "gob.short-cupboards", false);


    // Animal
    public static final IndirSetting<Boolean> FORAGEANIMALS = new IndirSetting<>(global, "gameplay.small-animaling-foraging", false);

    // Lighting
    public static final IndirSetting<Boolean> NIGHTVISION = new IndirSetting<>(global, "lighting.nightvision", false);
    public static final IndirSetting<Color> NVAMBIENTCOL = new IndirSetting<>(global, "lighting.nv-ambient-col", Color.WHITE);
    public static final IndirSetting<Color> NVDIFFUSECOL = new IndirSetting<>(global, "lighting.nv-diffuse-col", Color.WHITE);
    public static final IndirSetting<Color> NVSPECCOL = new IndirSetting<>(global, "lighting.nv-spec-col", Color.WHITE);
    public static final IndirSetting<Boolean> DARKMODE = new IndirSetting<>(global, "lighting.darkmode", false);

    //Camera
    public static final IndirSetting<String> CAMERA = new IndirSetting<>(global, "camera.camera-type", "sortho");
    public static final IndirSetting<Integer> CAMERAPROJFAR = new IndirSetting<>(global, "camera.camera-proj-far", 5000);
    public static final IndirSetting<Boolean> FREECAMREXAXIS = new IndirSetting<>(global, "camera.free.reverse-x-axis", false);
    public static final IndirSetting<Boolean> FREECAMREYAXIS = new IndirSetting<>(global, "camera.free.reverse-y-axis", false);
    public static final IndirSetting<Boolean> FREECAMLOCKELAV = new IndirSetting<>(global, "camera.free.lock-elevation", false);

    //Pathfinding
    public static final IndirSetting<Integer> PATHFINDINGTIER = new IndirSetting<>(global, "pathfinding.tier", 3);
    public static final IndirSetting<Boolean> LIMITPATHFINDING = new IndirSetting<>(global, "pathfinding.limit-distance-to-view", false);
    public static final IndirSetting<Boolean> RESEARCHUNTILGOAL = new IndirSetting<>(global, "pathfinding.research-until-at-goal", false);

    //Theme options
    public static final IndirSetting<String> HUDTHEME = new IndirSetting<>(global, "theme.hud", "default");
    public static final IndirSetting<Color> WNDCOL =
            new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.wnd.color", HUDTHEME),
                    Color.WHITE);
    public static final IndirSetting<Color> BTNCOL
            = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.button.color", HUDTHEME),
            Color.WHITE);
    public static final IndirSetting<Color> TXBCOL
            = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.textbox.color", HUDTHEME),
            Color.WHITE);
    public static final IndirSetting<Color> SLIDERCOL
            = new IndirSetting<>(global, new IndirSetting.IndirFormatKey("theme.%s.slider.color", HUDTHEME),
            Color.WHITE);

    //UI
    public static final IndirSetting<String> LANG = new IndirSetting<>(global, "ui.language", "english");
    public static final IndirSetting<Boolean> SHOWPLAVA = new IndirSetting<>(global, "ui.show-player-avatar", true);
    public static final IndirSetting<Boolean> SHOWSPEED = new IndirSetting<>(global, "ui.show-player-speed", true);
    public static final IndirSetting<Boolean> SHOWHEALTH = new IndirSetting<>(global, "ui.show-player-health", true);
    public static final IndirSetting<Boolean> SHOWENERGY = new IndirSetting<>(global, "ui.show-player-energy", true);
    public static final IndirSetting<Boolean> SHOWSTAM = new IndirSetting<>(global, "ui.show-player-stam", true);
    public static final IndirSetting<Boolean> SHOWCAL = new IndirSetting<>(global, "ui.show-calendar", true);
    public static final IndirSetting<Boolean> SHOWHOTBAR1 = new IndirSetting<>(global, "ui.show-hotbar1", true);
    public static final IndirSetting<Boolean> SHOWHOTBAR2 = new IndirSetting<>(global, "ui.show-hotbar2", true);
    public static final IndirSetting<Boolean> SHOWHOTBAR3 = new IndirSetting<>(global, "ui.show-hotbar3", true);
    public static final IndirSetting<Boolean> SHOWMINIINV = new IndirSetting<>(global, "ui.show-mini-inv", true);
    public static final IndirSetting<Boolean> SHOWMINIEQU = new IndirSetting<>(global, "ui.show-mini-equ", true);
    public static final IndirSetting<Boolean> SHOWSTUDY = new IndirSetting<>(global, "ui.show-study", true);
    public static final IndirSetting<Boolean> SHOWSESSIONS = new IndirSetting<>(global, "ui.show-session-display", true);
    public static final IndirSetting<Boolean> SHOWCHAT = new IndirSetting<>(global, "ui.show-chat", true);
    public static final IndirSetting<Boolean> SHOWLRSLOTS = new IndirSetting<>(global, "ui.show-lr-hand-slots", true);
    public static final IndirSetting<Boolean> SHOWMINIMAP = new IndirSetting<>(global, "ui.show-minimap", true);
    public static final IndirSetting<Boolean> SHOWINVONLOGIN = new IndirSetting<>(global, "ui.show-inv-on-login", true);
    public static final IndirSetting<Boolean> SHOWBELTONLOGIN = new IndirSetting<>(global, "ui.show-belt-on-login", true);
    public static final IndirSetting<Integer> MENUGRIDSIZEX = new IndirSetting<>(global, "ui.mg.size-x", 4);
    public static final IndirSetting<Integer> MENUGRIDSIZEY = new IndirSetting<>(global, "ui.mg.size-y", 4);
    public static final IndirSetting<Boolean> BIGSIMPLEMETERS = new IndirSetting<>(global, "ui.big-simple-imeters", false);
    public static final IndirSetting<Boolean> SHOWITEMQ = new IndirSetting<>(global, "ui.inv.show-item-quality", true);
    public static final IndirSetting<Boolean> SHOWITEMWEAR = new IndirSetting<>(global, "ui.inv.show-item-wear", true);
    public static final IndirSetting<Boolean> SHOWITEMCONT = new IndirSetting<>(global, "ui.inv.show-item-cont", true);
    public static final IndirSetting<Boolean> ALWAYSITEMLONGTIPS = new IndirSetting<>(global, "ui.inv.always-show-longtip", true);
    public static final IndirSetting<Boolean> AUTOEQUIP = new IndirSetting<>(global, "ui.inv.auto-equip", true);
    public static final IndirSetting<Boolean> WATERDROPITEMCTRL = new IndirSetting<>(global, "ui.dont-drop-item-over-water", false);
    public static final IndirSetting<Boolean> QUICKFLMENU = new IndirSetting<>(global, "ui.flowermenu.quick-menu", false);
    public static final IndirSetting<Boolean> AUTOONEOPTFMENU = new IndirSetting<>(global, "ui.flowermenu.auto-select-one-opt-menus", false);
    public static final IndirSetting<Boolean> KEEPFLOPEN = new IndirSetting<>(global, "ui.flowermenu.never-close-on-click", false);
    public static final IndirSetting<Boolean> SHOWEXPWND = new IndirSetting<>(global, "ui.show-experience-window", true);
    public static final IndirSetting<Boolean> SHOWMETERPER = new IndirSetting<>(global, "ui.show-per-for-meter", true);
    public static final IndirSetting<Boolean> SHOWTIMELEFTCURIO = new IndirSetting<>(global, "ui.show-time-left-for-curio", true);
    public static final IndirSetting<Boolean> SHOWEQUIPSTATS = new IndirSetting<>(global, "ui.show-equip-stats", true);

    //Hotkey related
    public static final IndirSetting<String> KB_F_STYLE = new IndirSetting<>(global, "keybind.hotkey-f-style", "GRID");
    public static final IndirSetting<Boolean> KB_F_VIS   = new IndirSetting<>(global, "keybind.hotkey-f-visible", true);
    public static final IndirSetting<Integer> KB_F_PAGE  = new IndirSetting<>(global, "keybind.hotkey-f-page", 0);
    public static final IndirSetting<Boolean> KB_F_LOCK  = new IndirSetting<>(global, "keybind.hotkey-f-lock",  false);
    public static final IndirSetting<String> KB_N_STYLE = new IndirSetting<>(global, "keybind.hotkey-n-style", "GRID");
    public static final IndirSetting<Boolean> KB_N_VIS   = new IndirSetting<>(global, "keybind.hotkey-n-visible", true);
    public static final IndirSetting<Integer> KB_N_PAGE  = new IndirSetting<>(global, "keybind.hotkey-n-page", 0);
    public static final IndirSetting<Boolean> KB_N_LOCK  = new IndirSetting<>(global, "keybind.hotkey-n-lock",  false);
    public static final IndirSetting<String> KB_STYLE = new IndirSetting<>(global, "keybind.hotkey-style", "GRID");
    public static final IndirSetting<Boolean> KB_VIS   = new IndirSetting<>(global, "keybind.hotkey-visible", true);
    public static final IndirSetting<Integer> KB_PAGE  = new IndirSetting<>(global, "keybind.hotkey-page", 0);
    public static final IndirSetting<Boolean> KB_LOCK  = new IndirSetting<>(global, "keybind.hotkey-lock",  false);
}
