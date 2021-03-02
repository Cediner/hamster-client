package hamster.ui.core.indir;

import hamster.GlobalSettings;
import haven.Resource;
import haven.TexI;

import java.awt.image.BufferedImage;

public class IndirThemeTex {
    private static final String fmt = "custom/hud/%s/%s";
    private final String res;
    private String theme;
    private final int id;
    private Resource cached;

    public IndirThemeTex(final String res, final int id) {
        this.res = res;
        this.id =  id;
        this.theme = "";
        this.cached = null;
    }

    private void checkTheme(final String ntheme) {
        if(!theme.equals(ntheme)) {
            this.theme = ntheme;
            this.cached = Resource.local().loadwait(String.format(fmt, theme, res));
        }
    }

    public TexI tex() {
        checkTheme(GlobalSettings.HUDTHEME.get());
        if (id == -1) {
            return cached.layer(Resource.imgc).texi();
        } else {
            return tex(id);
        }
    }

    public TexI tex(final int id) {
        checkTheme(GlobalSettings.HUDTHEME.get());
        for(final Resource.Image img : cached.layers(Resource.imgc)) {
            if(img.id == id)
                return img.texi();
        }
        throw new RuntimeException("Failed to find tex for " + String.format(fmt, theme, res) + " - id: " + id);
    }

    public BufferedImage img() {
        checkTheme(GlobalSettings.HUDTHEME.get());
        if (id == -1) {
            return cached.layer(Resource.imgc).img;
        } else {
            return img(id);
        }
    }

    public BufferedImage img(final int id) {
        checkTheme(GlobalSettings.HUDTHEME.get());
        for(final Resource.Image img : cached.layers(Resource.imgc)) {
            if(img.id == id)
                return img.img;
        }
        throw new RuntimeException("Failed to find tex for " + String.format(fmt, theme, res) + " - id: " + id);
    }
}
