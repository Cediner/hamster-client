package hamster.data.gob;

import java.util.Set;

public class ContainerData {
    public final Set<Integer> empty;
    public final Set<Integer> full;

    public ContainerData(final Set<Integer> empty, final Set<Integer> full) {
        this.empty = empty;
        this.full = full;
    }
}
