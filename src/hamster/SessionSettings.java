package hamster;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Session based settings based off account:character name
 */
public class SessionSettings {
    public static class Buff {
        public final IndirSetting<Boolean> status;
        public final String res;
        public final String menures;

        public Buff(final IndirSetting<Boolean> status, final String res, final String menures) {
            this.status = status;
            this.res = res;
            this.menures = menures;
        }
    }

    //Tracking server states of buffs
    //Seems like the server will remember: swimming, criminal acts, tracking, and visiting
    //In loftar's infinite wisdom he made it remember on an account level and not character...
    //But... if you fully log out it forgets these.  So we're going to assume you never Switch character and always
    // do a full logout  otherwise tracking this is very dumb.
    // Except Visting, visting is remembered even if you logout fully..
    public final IndirSetting<Boolean> SWIM;
    public final IndirSetting<Boolean> TRACKING;
    public final IndirSetting<Boolean> CRIMINALACTS;
    public final IndirSetting<Boolean> VISITING;
    public final IndirSetting<Boolean> PARTYPERMS;
    private final Map<String, Buff> buffmap = new HashMap<>();

    public SessionSettings(final String account, final String character) {
        final Settings local = new Settings(String.format("%s:%s", account, character));

        SWIM = new IndirSetting<>(local, "state.swimming", false);
        TRACKING = new IndirSetting<>(local, "state.tracking", false);
        CRIMINALACTS = new IndirSetting<>(local, "state.criminal-acts", false);
        VISITING = new IndirSetting<>(local, "state.visiting", false);
        PARTYPERMS = new IndirSetting<>(local, "state.party-perms", false);
        buffmap.put("Swimming", new Buff(SWIM, "custom/paginae/default/toggles/swim", "paginae/act/swim"));
        buffmap.put("Tracking", new Buff(TRACKING, "custom/paginae/default/toggles/tracking", "paginae/act/tracking"));
        buffmap.put("Visiting", new Buff(VISITING, "custom/paginae/default/toggles/nopeace", null));
        buffmap.put("Criminal", new Buff(CRIMINALACTS, "custom/paginae/default/toggles/crime", "paginae/act/crime"));
        buffmap.put("Party", new Buff(PARTYPERMS, "custom/paginae/default/toggles/permshare", "paginae/act/permshare"));
    }

    public Buff buff(final String name) {
        return buffmap.get(name);
    }

    public Collection<Buff> buffs() {
        return buffmap.values();
    }
}
