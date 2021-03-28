package hamster;

import com.google.common.flogger.FluentLogger;
import hamster.data.FarmingData;
import hamster.data.ForagableData;
import hamster.data.HighlightData;
import hamster.data.ItemData;
import hamster.gob.Alerted;
import hamster.gob.Deleted;
import hamster.gob.Hidden;
import hamster.gob.Tag;
import hamster.gob.attrs.monitors.RangeMonitor;
import hamster.io.Storage;
import hamster.script.LispScript;
import hamster.ui.chr.CredoTree;
import hamster.ui.chr.SkillTree;
import hamster.util.JobSystem;
import haven.JOGLPanel;

import java.awt.*;
import java.util.Optional;

/**
 * A list of settings that will work across all sessions
 */
public class GlobalSettings {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Settings global = new Settings("global");
    private static final Settings tmp = new Settings("gtmp", false);

    public static void init() {
        //preload lisp scripting config
        JobSystem.submit(LispScript::reloadConfig);
        FarmingData.init();
        final Optional<Storage> optint = Storage.create("jdbc:sqlite:data/static.sqlite");
        if (optint.isPresent()) {
            logger.atInfo().log("Loading alerted");
            Alerted.init(optint.get());
            logger.atInfo().log("Loading deleted");
            Deleted.init();
            logger.atInfo().log("Loading hidden");
            Hidden.init();
            logger.atInfo().log("Loading Tag Data");
            Tag.init(optint.get());
            logger.atInfo().log("Loading Range Data");
            RangeMonitor.init(optint.get());
            logger.atInfo().log("Loading highlighted");
            HighlightData.init();
            logger.atInfo().log("Loading itemdata");
            ItemData.init(optint.get());
            logger.atInfo().log("Loading foragables");
            ForagableData.init(optint.get());
            logger.atInfo().log("Loading Skill Data");
            SkillTree.init(optint.get());
            logger.atInfo().log("Loading Credo Data");
            CredoTree.init(optint.get());
            //Internal lookups are no longer needed
            optint.get().close();
        } else {
            logger.atSevere().log("Failed to open static datastore");
            System.exit(0);
        }
    }

    //Non-saved globals
    public static final IndirSetting<Boolean> PAUSED = new IndirSetting<>(tmp, "tmp.pause", false);
    public static final IndirSetting<Boolean> GENERATINGTOKEN = new IndirSetting<>(tmp, "generate-token", false);

    //General options
    public static final IndirSetting<Boolean> DEBUG = new IndirSetting<>(global, "system.debug", false);
    public static final IndirSetting<Boolean> SHOWSTATS = new IndirSetting<>(global, "system.show-stats", false);

    //Audio options
    public static final IndirSetting<Integer> MASTERVOL = new IndirSetting<>(global, "audio.master-volume", 1000);
    public static final IndirSetting<Integer> EVENTVOL = new IndirSetting<>(global, "audio.event-volume", 1000);
    public static final IndirSetting<Integer> AMBIENTVOL = new IndirSetting<>(global, "audio.ambient-volume", 1000);    ////Audio
    public static final IndirSetting<Integer> TIMERVOL = new IndirSetting<>(global,"audio.timer-volume", 1000);
    public static final IndirSetting<Integer> ALERTVOL = new IndirSetting<>(global, "audio.alert-volume", 1000);
    public static final IndirSetting<Integer> POPUPMSGVOL = new IndirSetting<>(global, "audio.popup-message-volume", 1000);
    public static final IndirSetting<Integer> ERRORMSGVOL = new IndirSetting<>(global, "audio.error-message-volume", 1000);

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

    // Animal
    public static final IndirSetting<Boolean> FORAGEANIMALS = new IndirSetting<>(global, "gameplay.small-animaling-foraging", false);

    // Lighting
    public static final IndirSetting<Boolean> NIGHTVISION = new IndirSetting<>(global, "lighting.nightvision", false);
    public static final IndirSetting<Color> NVAMBIENTCOL = new IndirSetting<>(global, "lighting.nv-ambient-col", Color.WHITE);
    public static final IndirSetting<Color> NVDIFFUSECOL = new IndirSetting<>(global, "lighting.nv-diffuse-col", Color.WHITE);
    public static final IndirSetting<Color> NVSPECCOL = new IndirSetting<>(global, "lighting.nv-spec-col", Color.WHITE);
    public static final IndirSetting<Boolean> DARKMODE = new IndirSetting<>(global, "lighting.darkmode", false);

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
