package hamster.data.itm;

/* This is a snapshot of a container with the type it's currently and content sizes */
public class ContainerData {
    public final ContainerType type;
    public final String name;
    public final double current;
    public final double max;

    public ContainerData(final ContainerType type, final String name,
		       final double size, final double max) {
	this.type = type;
	this.name = name;
	this.current = size;
	this.max = max;
    }
}
