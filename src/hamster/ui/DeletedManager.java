package hamster.ui;

import hamster.gob.Deleted;
import hamster.gob.Hidden;
import hamster.util.ObservableListener;
import haven.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class DeletedManager extends Window implements ObservableListener<String> {
    private final List<String> deleted = new ArrayList<>();
    private final TextEntry manualin;

    public DeletedManager() {
        super(Coord.z, "Deleted Manager", "Deleted Manager");
        Coord c = new Coord(0, 0);
        final Listbox<String> lst;
        c.y += add(lst = new Listbox<>(200, 20, 20) {
            @Override
            protected String listitem(int i) {
                return deleted.get(i);
            }

            @Override
            protected int listitems() {
                return deleted.size();
            }

            @Override
            protected void drawitem(GOut g, String item, int i) {
                g.text(item, new Coord(5, 1));
            }
        }, c.copy()).sz.y;
        manualin = add(new TextEntry(200, "", null, Hidden::add), c.copy());
        c.y += manualin.sz.y;
        c.y += add(new Button(200, "Add Deleted", () -> {
            if (!manualin.text.equals("")) {
                Deleted.add(manualin.text);
                OCache.MessageBus.send(new OCache.RemoveGobByRes(manualin.text));
            }
        }), c.copy()).sz.y;
        add(new Button(200, "Stop Deleting", () -> {
            if (lst.sel != null) {
                Deleted.remove(lst.sel);
            }
        }), c.copy());
        pack();
        hide();
        Deleted.listen(this);
    }

    @Override
    public void close() {
        hide();
    }

    @Override
    public void dispose() {
        Deleted.unlisten(this);
        super.dispose();
    }

    @Override
    public void init(Collection<String> base) {
        deleted.addAll(base);
        deleted.sort(String::compareTo);
    }

    @Override
    public void added(String item) {
        deleted.add(item);
        deleted.sort(String::compareTo);
    }

    @Override
    public void remove(String item) {
        deleted.remove(item);
    }
}
