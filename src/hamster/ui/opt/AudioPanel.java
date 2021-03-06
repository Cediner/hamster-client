package hamster.ui.opt;

import hamster.data.TranslationLookup;
import hamster.ui.core.Scrollport;
import hamster.ui.core.indir.*;
import hamster.ui.core.layout.LinearGrouping;
import haven.*;

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
	grp.add(new Label(TranslationLookup.get("opt_audio_combat_vol")));
	grp.add(new IndirHSlider(UI.scale(200), 0, 1000, COMBATSTARTVOL));
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
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_combat"), COMBATSTARTAUDIO));
	grp.add(new IndirLabel(() -> String.format("%s: %s", TranslationLookup.get("opt_audio_combat_res"), COMBATSTARTAUDIORES.get())));
	grp.add(new Button(TranslationLookup.get("opt_audio_change_combat_res"),
		() -> ui.root.add(new IndirSoundSelector("Combat Start", COMBATSTARTAUDIORES))));
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_chat"), ALLOWCHATSOUND));
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_failed_move"), SOUNDONFAILEDMOVE));
	grp.add(new IndirLabel(() -> String.format("%s: %s", TranslationLookup.get("opt_audio_failed_move_res"), QUEUEDMOVESTOP.get())));
	grp.add(new Button(TranslationLookup.get("opt_audio_change_failed_move_res"),
		() -> ui.root.add(new IndirSoundSelector("Failed queued move", QUEUEDMOVESTOP))));
	grp.add(new IndirCheckBox(TranslationLookup.get("opt_audio_allow_path_finish"), SOUNDONPATHFINISH));
	grp.add(new IndirLabel(() -> String.format("%s: %s", TranslationLookup.get("opt_audio_path_finish_res"), QUEUEDMOVESFINISH.get())));
	grp.add(new Button(TranslationLookup.get("opt_audio_change_path_finish_res"),
		() -> ui.root.add(new IndirSoundSelector("Path finish", QUEUEDMOVESFINISH))));
	grp.pack();

	int y = 0;
	add(grp, new Coord(0, y));
	pack();
    }
}
