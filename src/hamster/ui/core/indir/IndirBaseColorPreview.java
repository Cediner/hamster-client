package hamster.ui.core.indir;

import hamster.IndirSetting;
import hamster.ui.core.ColorPicker;
import haven.Coord;
import haven.GOut;
import haven.RichText;
import haven.Widget;
import haven.render.BaseColor;

import java.awt.*;
import java.util.function.Consumer;

public class IndirBaseColorPreview extends Widget {
    private final IndirSetting<BaseColor> col;
    private ColorPicker cp = null;
    private final Consumer<BaseColor> callback;

    public IndirBaseColorPreview(Coord sz, IndirSetting<BaseColor> cl, Consumer<BaseColor> callback) {
        super(sz);
        this.callback = callback;
        col = cl;
        settooltip(col.get().color());
    }

    public IndirBaseColorPreview(Coord sz, IndirSetting<BaseColor> cl) {
        this(sz, cl, null);
    }

    public void draw(GOut g) {
        g.chcolor(col.get().color());
        g.frect(Coord.z, sz);
        g.chcolor();
    }

    private void settooltip(final Color col) {
        tooltip = RichText.Parser.quote(String.format("Red: %d\nGreen: %d\nBlue: %d\nAlpha: %d",
                col.getRed(), col.getGreen(), col.getBlue(), col.getAlpha()));
        tooltip = RichText.render((String) tooltip, 200);
    }

    public boolean mousedown(Coord c, int btn) {
        return true;
    }

    public boolean mouseup(Coord c, int btn) {
        if (btn == 1 && cp == null) {
            cp = new ColorPicker(col.get().color(), (color -> {
                cp = null;
                col.set(new BaseColor(color));
                settooltip(col.get().color());
                if (callback != null)
                    callback.accept(col.get());
            }));

            if (ui.gui != null) {
                ui.gui.add(cp, new Coord(50, 50));
            } else {
                ui.root.add(cp, new Coord(50, 50));
            }
        }
        return true;
    }
}
