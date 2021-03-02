package hamster.ui.core.indir;

import hamster.IndirSetting;
import hamster.ui.core.Theme;
import hamster.ui.core.layout.HGridGrouping;
import haven.*;

import java.util.function.Consumer;

public class IndirRadioGroup<T> extends Widget {
    private static final IndirThemeRes res = Theme.themeres("chkbox/small");

    private static final int spacer = 5;

    private class RadioButton extends Widget {
        private final T condition;
        private final Text lbl;

        private RadioButton(final String name, final T condition) {
            this.condition = condition;
            this.lbl = Text.render(name);

            final Coord boxsz = res.tex(0).tex().sz();
            resize(new Coord(boxsz.x + spacer + this.lbl.sz().x, Math.max(boxsz.y, this.lbl.sz().y)));
        }

        public void draw(GOut g) {
            //Draw checkbox
            final int id = setting.get().equals(condition) ? 1 : 0;
            final Tex chk = res.tex(id).tex();
            g.image(chk, new Coord(0, sz.y / 2 - chk.sz().y / 2));
            //Draw label
            g.image(lbl.tex(), new Coord(chk.sz().x + spacer, sz.y / 2 - lbl.sz().y / 2));
            super.draw(g);
        }

        public boolean mousedown(Coord c, int button) {
            if (button == 1 && c.isect(Coord.z, res.tex(0).tex().sz())) {
                if (!setting.get().equals(condition)) { //Don't allow deselecting
                    setting.set(condition);
                    if (onChange != null)
                        onChange.accept(setting.get());
                }
                return true;
            }
            return false;
        }
    }

    private final IndirSetting<T> setting;
    private final Consumer<T> onChange;
    private final HGridGrouping grouping;

    public IndirRadioGroup(final String lbl, final int maxx, final IndirSetting<T> setting, final Consumer<T> callback) {
        this.setting = setting;
        this.onChange = callback;
        this.grouping = add(new HGridGrouping(lbl, new Coord(5, 5), 20, maxx, false));
    }

    public IndirRadioGroup(final String lbl, final int maxx, final IndirSetting<T> setting) {
        this(lbl, maxx, setting, null);
    }

    public void add(final String lbl, final T condition) {
        grouping.add(new RadioButton(lbl, condition));
        grouping.pack();
        pack();
    }
}
