package hamster.ui.Timer;
import hamster.data.TimerData;
import hamster.ui.core.NumberEntry;
import haven.*;


public class TimerEditWnd extends Window {
    TimerEditWnd(String cap) {
        super(Coord.z, cap, cap);
        final TextEntry txtname;
        final NumberEntry hours, mins, secs;

        int x = 15, y = 10;
        final int spacer = UI.scale(5);
        {
            final Label lbl = add(new Label("Name"), new Coord(x, y));
            txtname = add(new TextEntry(UI.scale(200), ""), new Coord(lbl.c.x, lbl.c.y + lbl.sz.y + spacer));
            x = txtname.c.x + txtname.sz.x + spacer;
        }
        {
            final Label lbl = add(new Label("Hours"), new Coord(x, y));
            hours = add(new NumberEntry(UI.scale(75), 0, 0, 168), new Coord(lbl.c.x, lbl.c.y + lbl.sz.y + spacer));
            x = hours.c.x + hours.sz.x + spacer;
        }
        {
            final Label lbl = add(new Label("Minutes"), new Coord(x, y));
            mins = add(new NumberEntry(UI.scale(75), 0, 0, 60), new Coord(lbl.c.x, lbl.c.y + lbl.sz.y + spacer));
            x = mins.c.x + mins.sz.x + spacer;
        }
        {
            final Label lbl = add(new Label("Second"), new Coord(x, y));
            secs = add(new NumberEntry(UI.scale(75), 0, 0, 60), new Coord(lbl.c.x, lbl.c.y + lbl.sz.y + spacer));
        }

        Button add = new Button(UI.scale(60), "Add", () -> {
            long duration = ((3600 * hours.value()) + (60 * mins.value()) + secs.value());
            TimerData.addTimer(txtname.text, duration);
            ui.destroy(this);
        });
        add(add, new Coord(txtname.c.x, txtname.c.y + txtname.sz.y + spacer));

        Button cancel = new Button(UI.scale(60), "Cancel") {
            @Override
            public void click() {
                parent.reqdestroy();
            }
        };
        add(cancel, new Coord(secs.c.x + secs.sz.x - cancel.sz.x, secs.c.y + secs.sz.y + spacer));
        pack();
        resize(csz.add(15,10));
    }

    public void close() {
        ui.destroy(this);
    }
}
