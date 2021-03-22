package hamster;

import com.google.common.flogger.FluentLogger;
import hamster.data.ForagableData;
import hamster.data.HighlightData;
import hamster.data.ItemData;
import hamster.gob.Alerted;
import hamster.gob.Deleted;
import hamster.gob.Hidden;
import hamster.gob.Tag;
import hamster.io.Storage;
import hamster.ui.chr.CredoTree;
import hamster.ui.chr.SkillTree;
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
        final Optional<Storage> optint = Storage.create("jdbc:sqlite:data/static.sqlite");
        if (optint.isPresent()) {
            //logger.atInfo().log("Loading growth");
            //Growth.init(optint.get());
            //logger.atInfo().log("Loading range");
            //Range.init(optint.get());
            logger.atInfo().log("Loading alerted");
            Alerted.init(optint.get());
            logger.atInfo().log("Loading deleted");
            Deleted.init();
            logger.atInfo().log("Loading hidden");
            Hidden.init();
            logger.atInfo().log("Loading Tag Data");
            Tag.init(optint.get());
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

    //General options
    public static final IndirSetting<Boolean> DEBUG = new IndirSetting<>(global, "system.debug", false);
    public static final IndirSetting<Boolean> SHOWSTATS = new IndirSetting<>(global, "system.show-stats", false);

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
