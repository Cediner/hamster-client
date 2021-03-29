package hamster.data.itm;

import java.util.Set;

public class GlidData {
    private int min_chance, max_chance;
    private Set<Attribute> attributes;

    public int min_chance() { return min_chance; }
    public int max_chance() { return max_chance; }
    public Set<Attribute> attributes() { return attributes; }
}
