package hamster.ui;

import hamster.gob.Hidden;
import hamster.util.ObservableListener;
import haven.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class HiddenManager extends Window implements ObservableListener<String> {
    private final List<String> hidden = new ArrayList<>();
    private final TextEntry manualin;


    public HiddenManager() {
        super(Coord.z, "Hidden Manager", "Hidden Manager");
        Coord c = new Coord(0, 0);
        final Listbox<String> lst;
        c.y += add(lst = new Listbox<>(200, 20, 20) {
            @Override
            protected String listitem(int i) {
                return hidden.get(i);
            }

            @Override
            protected int listitems() {
                return hidden.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, new Coord(5, 1));
            }
        }, c.copy()).sz.y;
        manualin = add(new TextEntry(200, "", null, Hidden::add), c.copy());
        c.y += manualin.sz.y;
        c.y += add(new Button(200, "Add Hidden", () -> {
            if (!manualin.text.equals("")) {
                Hidden.add(manualin.text);
                OCache.OCMessageBus.send(new OCache.HideGobsByName(manualin.text));
            }
        }), c.copy()).sz.y;
        add(new Button(200, "Stop Hiding", () -> {
            if (lst.sel != null) {
                Hidden.remove(lst.sel);
                OCache.OCMessageBus.send(new OCache.UnhideGobsByName(lst.sel));
            }
        }), c.copy());
        pack();
        hide();
        Hidden.listen(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void dispose() {
        Hidden.unlisten(this);
        super.dispose();
    }

    @Override
    public void init(Collection<String> base) {
        hidden.addAll(base);
        hidden.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
        hidden.add(item);
        hidden.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
        hidden.remove(item);
    }
}
