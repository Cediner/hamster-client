package hamster.data.gob;

import haven.*;

import java.util.HashMap;
import java.util.Map;

public class ForageData {
    private static final Map<Status, Text.Line> seasonstatus = new HashMap<>();
    public enum Status {
        YES, NO, MAYBE
    }

    static {
        seasonstatus.put(Status.YES, Text.renderstroked("Yes"));
        seasonstatus.put(Status.NO, Text.renderstroked("No"));
        seasonstatus.put(Status.MAYBE, Text.renderstroked("Maybe"));
    }

    private Text name;
    private RichText location_txt;
    private String location;
    private Indir<Resource> res;
    private int min_val;
    private int max_val;
    private Status spring, summer, autumn, winter;

    public void init(final ObjData obj) {
        this.name = Text.render(obj.name());
        this.location_txt = RichText.render(
                String.format("Found in:\n  \u2022 %s", location.replaceAll(",\\s+", "\n  \u2022 ")),
                200);
        if (obj.res() != null && !obj.res().equals("")) {
            this.res = Resource.remote().load(obj.res());
        } else {
            this.res = null;
        }
    }

    public Text name() { return name; }
    public Indir<Resource> res() { return res; }
    public RichText location() { return location_txt; }
    public int min_value() { return min_val; }
    public int max_value() { return max_val; }
    public Tex spring() { return seasonstatus.get(spring).tex(); }
    public Tex summer() { return seasonstatus.get(summer).tex(); }
    public Tex autumn() { return seasonstatus.get(autumn).tex(); }
    public Tex winter() { return seasonstatus.get(winter).tex(); }
}
