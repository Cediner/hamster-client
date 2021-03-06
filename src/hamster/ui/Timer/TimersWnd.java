package hamster.ui.Timer;


import hamster.data.TimerData;
import hamster.util.ObservableListener;
import haven.*;

import java.util.Collection;

public class TimersWnd extends Window implements ObservableListener<TimerData.Timer> {
    public static final int width = 460;

    public TimersWnd() {
        super(Coord.z, "Timers", "Timers");

        add(new Button(UI.scale(150), "Add", () ->
                parent.parent.add(new TimerEditWnd("Create New Timer"),
                        new Coord(ui.gui.sz.x / 2 - 200, ui.gui.sz.y / 2 - 200))));
        pack();
    }

    @Override
    protected void added() {
        super.added();
        TimerData.listenTimers(this);
    }

    @Override
    public void init(Collection<TimerData.Timer> base) {
        for (final TimerData.Timer timer : base) {
            TimersWnd.this.add(new TimerWdg(timer));
        }
        pack();
    }

    @Override
    public void added(TimerData.Timer item) {
        TimersWnd.this.add(new TimerWdg(item));
        pack();
    }

    @Override
    public void remove(TimerData.Timer item) {
        ui.destroy(find(item));
        pack();
    }

    public TimerWdg find(TimerData.Timer t) {
        Widget next;

        for (Widget wdg = child; wdg != null; wdg = next) {
            next = wdg.next;
            if (wdg instanceof TimerWdg) {
                TimerWdg tw = (TimerWdg) wdg;
                if (tw.time == t)
                    return tw;
            }
        }

        return null;
    }

    public void pack() {
        int y = 35;
        Widget next;

        for (Widget wdg = child; wdg != null; wdg = next) {
            next = wdg.next;
            if (wdg instanceof TimerWdg) {
                wdg.c = new Coord(wdg.c.x, y);
                y += wdg.sz.y;
            }
        }
        super.pack();
    }

    public void close() {
        hide();
    }

    @Override
    protected void removed() {
        TimerData.removeTimerListener(this);
    }
}
