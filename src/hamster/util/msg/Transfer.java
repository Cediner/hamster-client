package hamster.util.msg;

public class Transfer extends Message {
    public final Runnable job;
    public Transfer(final Runnable job) {
	this.job = job;
    }

    public void transfer() {
	job.run();
    }
}