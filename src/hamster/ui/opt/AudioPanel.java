package hamster.ui.opt;

import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.IndirCheckBox;
import hamster.ui.core.indir.IndirHSlider;
import hamster.ui.core.indir.IndirLabel;
import hamster.ui.core.indir.IndirRadioGroup;
import hamster.ui.core.layout.Grouping;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

import java.util.function.Consumer;

import static hamster.GlobalSettings.*;


public class AudioPanel extends Scrollport {
    public AudioPanel(final UI ui) {
	super(new Coord(UI.scale(500), UI.scale(395)));
	final Coord spacer = new Coord(UI.scale(20), UI.scale(5));
	final LinearGrouping grp = new LinearGrouping("Audio Settings", spacer, false);

	grp.add(new Label("Master audio volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, MASTERVOL, (val) -> Audio.setvolume(val / 1000.0)));
	grp.add(new Label("In-game event volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, EVENTVOL, (val) -> ui.audio.pos.setvolume(val / 1000.0)));
	grp.add(new Label("Ambient volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, AMBIENTVOL, (val) -> ui.audio.amb.setvolume(val / 1000.0)));
	grp.add(new Label("Timer volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, TIMERVOL));
	grp.add(new Label("Alert volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, ALERTVOL));
	grp.add(new Label("Error Message volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, ERRORMSGVOL));
	grp.add(new Label("Popup Message volume"));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, POPUPMSGVOL));
	grp.add(new IndirCheckBox("Allow Sound on Popup Message", SOUNDONPOPUPMSG));
	grp.add(new IndirCheckBox("Allow Sound from Gobs", SOUNDONGOBAUDIO));
	grp.pack();

	int y = 0;
	add(grp, new Coord(0, y));
	pack();
    }
}
