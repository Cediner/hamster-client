package hamster.ui.core;

import hamster.GlobalSettings;
import hamster.gfx.TextureAtlas;
import hamster.ui.core.indir.IndirThemeRes;
import hamster.ui.core.indir.IndirThemeTex;
import haven.Coord;
import haven.Resource;
import haven.Tex;

import java.awt.image.BufferedImage;

/**
 * Shortcuts for getting theme'd resource files
 */
public class Theme {
    private static final TextureAtlas atlas = new TextureAtlas("Theme", new Coord(512, 512));
    private static final String fmt = "custom/hud/%s/%s";

    public static IndirThemeTex themetex(final String res) {
        return new IndirThemeTex(res,  -1, atlas);
    }
    public static IndirThemeTex themetex(final String res, final int id) {
        return new IndirThemeTex(res,  id, atlas);
    }
    public static IndirThemeRes themeres(final String res) {
        return new IndirThemeRes(res, atlas);
    }

    public static Tex tex(final String res) {
        return Resource.loadtex(String.format(fmt, GlobalSettings.HUDTHEME.get(), res));
    }

    public static Tex tex(final String res, final int id) {
        return Resource.loadtex(String.format(fmt, GlobalSettings.HUDTHEME.get(), res), id);
    }

    public static BufferedImage img(final String res) {
        return Resource.loadimg(String.format(fmt, GlobalSettings.HUDTHEME.get(), res));
    }

    public static BufferedImage img(final String res, final int id) {
        return Resource.loadimg(String.format(fmt, GlobalSettings.HUDTHEME.get(), res), id);
    }

    public static Resource res(final String res) {
        return Resource.local().loadwait(String.format(fmt, GlobalSettings.HUDTHEME.get(), res));
    }

    public static String fullres(final String res) {
        return String.format(fmt, GlobalSettings.HUDTHEME.get(), res);
    }
}
