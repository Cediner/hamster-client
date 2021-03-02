package hamster;

import java.awt.*;

/**
 * A list of settings that will work across all sessions
 */
public class GlobalSettings {
    private static final Settings global = new Settings("global");
    private static final Settings tmp = new Settings("gtmp", false);

    //Non-saved globals
    public static final IndirSetting<Boolean> PAUSED = new IndirSetting<>(tmp, "tmp.pause", false);

    //Display options
    public static final IndirSetting<Boolean> VSYNC = new IndirSetting<>(global, "display.vsync", true);
    public static final IndirSetting<Integer> FPS = new IndirSetting<>(global, "display.fps", 60);
    public static final IndirSetting<Integer> BGFPS = new IndirSetting<>(global, "display.bg-fps", 5);
    public static final IndirSetting<Boolean> SHADOWS = new IndirSetting<>(global, "display.shadows.show", true);
    public static final IndirSetting<Integer> SHADOWQUALITY = new IndirSetting<>(global, "display.shadows.quality", 4);
    public static final IndirSetting<Integer> SHADOWSIZE = new IndirSetting<>(global, "display.shadows.size", 3);
    public static final IndirSetting<Integer> SHADOWDEPTH = new IndirSetting<>(global, "display.shadows.depth", 3);
    public static final IndirSetting<Boolean> SYMMETRICOUTLINES = new IndirSetting<>(global, "display.outlines.symmetric", false);

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
