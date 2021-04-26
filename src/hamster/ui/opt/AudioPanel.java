package hamster.ui.opt;

import hamster.data.TranslationLookup;
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
	super(OptionsWnd.PANEL_SIZE);
	final Coord spacer = new Coord(UI.scale(20), UI.scale(5));
	final LinearGrouping grp = new LinearGrouping(TranslationLookup.get("opt_audio_settings"), spacer, false);

	grp.add(new Label(TranslationLookup.get("opt_audio_master_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, MASTERVOL, (val) -> Audio.setvolume(val / 1000.0)));
	grp.add(new Label(TranslationLookup.get("opt_audio_event_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, EVENTVOL, (val) -> ui.audio.pos.setvolume(val / 1000.0)));
	grp.add(new Label(TranslationLookup.get("opt_audio_ambient_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, AMBIENTVOL, (val) -> ui.audio.amb.setvolume(val / 1000.0)));
	grp.add(new Label(TranslationLookup.get("opt_audio_ui_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, UIVOL, (val) -> ui.audio.aui.setvolume(val / 1000.0)));
	grp.add(new Label(TranslationLookup.get("opt_audio_timer_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, TIMERVOL));
	grp.add(new Label(TranslationLookup.get("opt_audio_alert_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, ALERTVOL));
	grp.add(new Label(TranslationLookup.get("opt_audio_error_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, ERRORMSGVOL));
	grp.add(new Label(TranslationLookup.get("opt_audio_popup_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, POPUPMSGVOL));
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_popup"), SOUNDONPOPUPMSG));
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_gob"), SOUNDONGOBAUDIO));
	grp.pack();

	int y = 0;
	add(grp, new Coord(0, y));
	pack();
    }
}
