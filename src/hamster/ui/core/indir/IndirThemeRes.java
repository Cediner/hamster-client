package hamster.ui.core.indir;

import hamster.GlobalSettings;
import haven.Resource;


public class IndirThemeRes {
    private static final String fmt = "custom/hud/%s/%s";
    private final String res;
    private String theme;
    private Resource cached;

    public IndirThemeRes(final String res) {
        this.res = res;
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
        return new IndirThemeTex(res, -1);
    }

    public IndirThemeTex tex(final int id) {
        return new IndirThemeTex(res, id);
    }

    public IndirThemeTex img() {
        return new IndirThemeTex(res, -1);
    }

    public IndirThemeTex img(final int id) {
        return new IndirThemeTex(res, id);
    }
}
