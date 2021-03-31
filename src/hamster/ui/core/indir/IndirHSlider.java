package hamster.ui.core.indir;

import hamster.GlobalSettings;
import hamster.IndirSetting;
import hamster.ui.core.Theme;
import haven.Coord;
import haven.GOut;
import haven.UI;
import haven.Widget;

import java.util.function.Consumer;

public class IndirHSlider extends Widget {
    private static final IndirThemeRes res = Theme.themeres("scroll/horizontal");
    private static final IndirThemeTex left = res.tex(0);
    private static final IndirThemeTex middle = res.tex(1);
    private static final IndirThemeTex right = res.tex(2);
    private static final IndirThemeTex slider = res.tex(3);

    private final int min, max;
    private final IndirSetting<Integer> val;
    private final Consumer<Integer> onChange;
    private UI.Grab drag = null;

    public IndirHSlider(int w, int min, int max, IndirSetting<Integer> val, final Consumer<Integer> onChange) {
        super(new Coord(w, slider.tex().sz().y));
        this.val = val;
        this.min = min;
        this.max = max;
        this.onChange = onChange;
    }

    public IndirHSlider(int w, int min, int max, IndirSetting<Integer> val) {
        this(w, min, max, val, null);
    }

    public void draw(GOut g) {
        g.chcolor(GlobalSettings.SLIDERCOL.get());
        //y offset incase sflarp.sz.y > schain.sz.y
        int cy = (slider.tex().sz().y / 2) - (left.tex().sz().y / 2);
        //Top
        g.image(left.tex(), new Coord(0, cy));
        //middle
        g.rimageh(middle.tex(), new Coord(left.tex().sz().x, cy), sz.x - (left.tex().sz().x + right.tex().sz().x));
        //bottom
        g.image(right.tex(), new Coord(sz.x - right.tex().sz().x, cy));
        //slider
        int fx = ((sz.x - slider.tex().sz().x) * (val.get() - min)) / (max - min);
        g.image(slider.tex(), new Coord(fx, 0));
        g.chcolor();
    }

    public boolean mousedown(Coord c, int button) {
        if (button != 1 || !c.isect(Coord.z, sz))
            return (false);
        drag = ui.grabmouse(this);
        mousemove(c);
        return (true);
    }

    public void mousemove(Coord c) {
        if (drag != null) {
            double a = (double) (c.x - (slider.tex().sz().x / 2)) / (double) (sz.x - slider.tex().sz().x);
            if (a < 0)
                a = 0;
            if (a > 1)
                a = 1;
            val.set((int) Math.round(a * (max - min)) + min);
            if (onChange != null)
                onChange.accept(val.get());
        }
    }

    public boolean mouseup(Coord c, int button) {
        if (button != 1)
            return (false);
        if (drag == null)
            return (false);
        drag.remove();
        drag = null;
        return (true);
    }

    public void resize(int w) {
        super.resize(new Coord(w, slider.tex().sz().y));
    }
}
