package hamster.ui.core.indir;

import hamster.GlobalSettings;
import hamster.gfx.TextureAtlas;
import haven.Resource;


public class IndirThemeRes {
    private static final String fmt = "custom/hud/%s/%s";
    private final String res;
    private final TextureAtlas atlas;
    private String theme;
    private Resource cached;

    public IndirThemeRes(final String res, final TextureAtlas atlas) {
        this.res = res;
        this.atlas = atlas;
        this.theme = "";
        this.cached = null;
    }

    private void checkTheme(final String ntheme) {
        if(!theme.equals(ntheme)) {
            this.theme = ntheme;
            this.cached = Resource.local().loadwait(String.format(fmt, theme, res));
        }
    }

    public <L extends Resource.Layer> L layer(Class<L> cl) {
        checkTheme(GlobalSettings.HUDTHEME.get());
        return cached.layer(cl);
    }

    public IndirThemeTex tex() {
        return new IndirThemeTex(res, -1, atlas);
    }

    public IndirThemeTex tex(final int id) {
        return new IndirThemeTex(res, id, atlas);
    }

    public IndirThemeTex img() {
        return new IndirThemeTex(res, -1, atlas);
    }

    public IndirThemeTex img(final int id) {
        return new IndirThemeTex(res, id, atlas);
    }
}
